{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Update (runUpdate, update) where

import           Control.Monad             (guard, replicateM, unless, when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Random      (MonadRandom, getRandomR)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Monad.Trans.RWS   (RWST (..), execRWST, get, put, tell)
import           Data.Maybe                (mapMaybe)
import qualified Data.Set                  as Set
import qualified Data.Time                 as Time
import qualified Graphics.UI.SDL           as SDL
import qualified Graphics.Vty              as Vty
import           System.Random.Shuffle     (shuffleM)

import           Audio
import           Core

-- | Assign tasks randomly so each player has at least one task,
-- and each task has at least one player.
assignTasks :: (MonadRandom m) => Phase -> m Phase
assignTasks p = do
  tasks   <- shuffleM $ map fst             $ phaseTasks   p
  players <- shuffleM $ zipWith const [0..] $ phasePlayers p
  let assignment = if
        | null players || null tasks    -> []
        | length tasks < length players -> zip (cycle tasks) players
        | otherwise                     -> zip tasks (cycle players)
      indexesFor task = [ i | (t, i) <- assignment, t == task ]
      newMapping = [ (t, indexesFor t) | (t, _) <- phaseTasks p ]
  return $ p { phaseTasks = newMapping }

type Update = RWST () (Set.Set SFX) Phase (MaybeT IO)

runUpdate :: Update a -> Phase -> IO (Maybe (Phase, Set.Set SFX))
runUpdate u = runMaybeT . execRWST u ()

playSFX :: SFX -> Update ()
playSFX = tell . Set.singleton

abort :: Update a
abort = RWST $ \() _ -> MaybeT $ return Nothing

update
  :: [(SDL.JoystickID, Button360)]
  -> [Vty.Key]
  -> [(String, Bool)]
  -> Update ()
update sdl keys api = do
  phase <- get
  now <- liftIO Time.getCurrentTime
  let next = put
      pressedChar c = Vty.KChar c `elem` keys
      pressedKey k = k `elem` keys
      inUse (joy, btn) = flip any (phasePlayers phase) $ \case
        PlayerJoy{..} -> playerJoystick == joy && elem btn [playerYes, playerNo]
        PlayerAPI{}   -> False
      pressedYes = \case
        PlayerJoy{..} -> (playerJoystick, playerYes) `elem` sdl
        PlayerAPI{..} -> (playerCode    , True     ) `elem` api
      pressedNo = \case
        PlayerJoy{..} -> (playerJoystick, playerNo ) `elem` sdl
        PlayerAPI{..} -> (playerCode    , False    ) `elem` api
  case phase of
    Waiting{..}
      | pressedChar '3' -> next AddPlayerYes{..}
      | pressedChar 'a' -> next AddPlayerAPI{ phaseName = "", .. }
      | pressedChar '=' -> next AddTask{ phaseNewTask = "", .. }
      | pressedKey Vty.KEsc -> next ConfirmQuit{..}
      | pressedKey Vty.KDel && not (null phasePlayers)
        -> next DeletePlayer{ phaseIndex = 0, .. }
      | pressedChar '-' && not (null phaseTasks)
        -> next DeleteTask{ phaseIndex = 0, .. }
      | pressedChar 'l' -> assignTasks phase >>= next
      | pressedChar 'v' -> next Voting
        { phasePlayersYes = []
        , phasePlayersNo  = []
        , phaseTimeStart  = now
        , phaseTimeNow    = now
        , phaseVoteLength = 10 -- seconds
        , ..
        }
      | pressedChar ' ' -> next Inspection
        { phasePlayersGood = []
        , phasePlayersBad  = []
        , phaseTimeStart   = now
        , phaseTimeNow     = now
        , ..
        }
      | pressedChar '1' && not (null phasePlayers) -> do
        phaseIndex <- getRandomR (0, length phasePlayers - 1)
        next ChosenOne{..}
      | pressedChar 'c' && not (null phasePlayers)
        -> next Citation{ phaseIndex = 0, .. }
      | otherwise -> return ()
    ConfirmQuit{..}
      | pressedChar 'y' -> abort
      | pressedChar 'n' -> next Waiting{..}
      | otherwise       -> return ()
    AddPlayerYes{..} -> next $ case filter (not . inUse) sdl of
      (phaseJoystick, phaseYes) : _ -> AddPlayerNo{..}
      []  | pressedKey Vty.KEsc     -> Waiting{..}
          | otherwise               -> phase
    AddPlayerNo{..} -> next $ let
      buttons = do
        (joy', btnNo) <- sdl
        guard $ not $ inUse (joy', btnNo)
        guard $ phaseJoystick == joy'
        guard $ btnNo /= phaseYes
        return btnNo
      in case buttons of
        buttonNo : _ -> AddPlayerName{ phaseName = "", phaseNo = buttonNo, .. }
        []  | pressedKey Vty.KEsc -> AddPlayerYes{..}
            | otherwise           -> phase
    AddPlayerName{..} -> let
      funs = flip map keys $ \case
        Vty.KBS     -> \s -> take (length s - 1) s
        Vty.KChar c -> (++ [c])
        _           -> id
      name' = foldl (flip ($)) phaseName funs
      newPlayer = PlayerJoy
        { playerName      = name'
        , playerCitations = 0
        , playerJoystick  = phaseJoystick
        , playerYes       = phaseYes
        , playerNo        = phaseNo
        }
      in if
        | pressedKey Vty.KEnter -> do
          playSFX SFX_pinball_8
          next Waiting{ phasePlayers = phasePlayers ++ [newPlayer], .. }
        | pressedKey Vty.KEsc -> next AddPlayerNo{..}
        | otherwise -> next AddPlayerName{ phaseName = name', .. }
    AddPlayerAPI{..} -> let
      funs = flip map keys $ \case
        Vty.KBS     -> \s -> take (length s - 1) s
        Vty.KChar c -> (++ [c])
        _           -> id
      name' = foldl (flip ($)) phaseName funs
      newPlayer = do
        code <- newCode
        return PlayerAPI
          { playerName      = name'
          , playerCitations = 0
          , playerCode      = code
          }
      usedCodes = do
        PlayerAPI{..} <- phasePlayers
        return playerCode
      newCode = do
        code <- replicateM 4 $ getRandomR ('A', 'Z')
        if code `elem` usedCodes
          then newCode
          else return code
      in if
        | pressedKey Vty.KEnter -> do
          player <- newPlayer
          playSFX SFX_pinball_8
          next Waiting{ phasePlayers = phasePlayers ++ [player], .. }
        | pressedKey Vty.KEsc -> next Waiting{..}
        | otherwise -> next AddPlayerAPI{ phaseName = name', .. }
    DeletePlayer{..} -> next $ if
      | pressedKey Vty.KUp    ->
        DeletePlayer{ phaseIndex = max 0 $ phaseIndex - 1, .. }
      | pressedKey Vty.KDown  ->
        DeletePlayer{ phaseIndex = min (length phasePlayers - 1) $ phaseIndex + 1, .. }
      | pressedKey Vty.KEsc   -> Waiting{..}
      | pressedKey Vty.KEnter -> Waiting
        { phasePlayers = case splitAt phaseIndex phasePlayers of
          (xs, ys) -> xs ++ drop 1 ys
        , phaseTasks = do
          let adjustIndex i = case compare i phaseIndex of
                EQ -> Nothing
                LT -> Just i
                GT -> Just $ i - 1
          (task, playerIndices) <- phaseTasks
          return (task, mapMaybe adjustIndex playerIndices)
        , ..
        }
      | otherwise -> phase
    AddTask{..} -> let
      funs = flip map keys $ \case
        Vty.KBS     -> \s -> take (length s - 1) s
        Vty.KChar c -> (++ [c])
        _           -> id
      task' = foldl (flip ($)) phaseNewTask funs
      in if
        | pressedKey Vty.KEnter -> do
          playSFX SFX_pinball_8
          next Waiting{ phaseTasks = phaseTasks ++ [(task', [])], .. }
        | pressedKey Vty.KEsc   -> next Waiting{..}
        | otherwise             -> next AddTask{ phaseNewTask = task', .. }
    DeleteTask{..} -> next $ if
      | pressedKey Vty.KUp    ->
        DeleteTask{ phaseIndex = max 0 $ phaseIndex - 1, .. }
      | pressedKey Vty.KDown  ->
        DeleteTask{ phaseIndex = min (length phaseTasks - 1) $ phaseIndex + 1, .. }
      | pressedKey Vty.KEsc   -> Waiting{..}
      | pressedKey Vty.KEnter -> Waiting
        { phaseTasks = case splitAt phaseIndex phaseTasks of
          (xs, ys) -> xs ++ drop 1 ys
        , ..
        }
      | otherwise -> phase
    Voting{..} -> do
      let undecided = do
            (playerIndex, player) <- zip [0..] phasePlayers
            guard $ notElem playerIndex $ phasePlayersYes ++ phasePlayersNo
            return (playerIndex, player)
          newYes = phasePlayersYes ++ addedYes
          addedYes = [ ix | (ix, player) <- undecided, pressedYes player ]
          newNo = phasePlayersNo ++ addedNo
          addedNo = [ ix | (ix, player) <- undecided, pressedNo player, ix `notElem` addedYes ]
      unless (null addedYes) $ playSFX SFX_monster_yes
      unless (null addedNo) $ playSFX SFX_monster_no
      if  | pressedKey Vty.KEsc -> next Waiting{..}
          | Time.diffUTCTime now phaseTimeStart >= phaseVoteLength -- time's up
            || length newYes + length newNo == length phasePlayers -- everyone's voted
            || pressedChar 'v'                                     -- ended vote early
            -> do
              when (null addedYes && null addedNo) $ playSFX SFX_time_up
              next VoteComplete
                { phasePlayersYes = newYes
                , phasePlayersNo  = newNo
                , ..
                }
          | otherwise -> next Voting
            { phaseTimeNow    = now
            , phasePlayersYes = newYes
            , phasePlayersNo  = newNo
            , ..
            }
    VoteComplete{..} -> next $ if
      | pressedKey Vty.KEsc -> Waiting{..}
      | otherwise           -> phase
    Inspection{..} -> do
      let undecided = do
            (playerIndex, player) <- zip [0..] phasePlayers
            guard $ notElem playerIndex $ map fst $ phasePlayersGood ++ phasePlayersBad
            return (playerIndex, player)
          newGood = phasePlayersGood ++ addedGood
          addedGood = [ (ix, now) | (ix, player) <- undecided, pressedYes player ]
          newBad = phasePlayersBad ++ addedBad
          addedBad = [ (ix, now) | (ix, player) <- undecided, pressedNo player, ix `notElem` map fst addedGood ]
      unless (null addedGood) $ playSFX SFX_stamp_down
      unless (null addedBad) $ playSFX SFX_border_callguards
      next $ if
        | pressedKey Vty.KEsc -> Waiting{..}
        | otherwise -> Inspection
          { phaseTimeNow     = now
          , phasePlayersGood = newGood
          , phasePlayersBad  = newBad
          , ..
          }
    ChosenOne{..} -> next $ if
      | pressedKey Vty.KEsc -> Waiting{..}
      | otherwise           -> phase
    Citation{..} -> if
      | pressedKey Vty.KUp    ->
        next Citation{ phaseIndex = max 0 $ phaseIndex - 1, .. }
      | pressedKey Vty.KDown  ->
        next Citation{ phaseIndex = min (length phasePlayers - 1) $ phaseIndex + 1, .. }
      | pressedKey Vty.KEsc   -> next Waiting{..}
      | pressedKey Vty.KEnter -> do
        playSFX SFX_printer_line
        next Waiting
          { phasePlayers = case splitAt phaseIndex phasePlayers of
            (xs, player : ys) -> let
              cited = player{ playerCitations = playerCitations player + 1 }
              in xs ++ [cited] ++ ys
            (_ , []         ) -> phasePlayers -- shouldn't happen
          , ..
          }
      | otherwise -> return ()
