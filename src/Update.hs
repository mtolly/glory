{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Update (update) where

import           Control.Applicative   ((<$>))
import           Control.Monad         (guard, replicateM)
import           Control.Monad.Random  (MonadRandom, getRandomR)
import           Data.Maybe            (mapMaybe)
import qualified Data.Time             as Time
import qualified Graphics.UI.SDL       as SDL
import qualified Graphics.Vty          as Vty
import           System.Random.Shuffle (shuffleM)

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

update :: [(SDL.JoystickID, Button360)] -> [Vty.Key] -> [(String, Bool)] -> Phase -> IO (Maybe Phase)
update sdl keys api phase = case phase of
  Waiting{..}
    | pressedChar '3' -> next AddPlayerYes{..}
    | pressedChar 'a' -> next AddPlayerAPI{ phaseName = "", .. }
    | pressedChar '=' -> next AddTask{ phaseNewTask = "", .. }
    | pressedKey Vty.KEsc -> next ConfirmQuit{..}
    | pressedKey Vty.KDel && not (null phasePlayers)
      -> next DeletePlayer{ phaseIndex = 0, .. }
    | pressedChar '-' && not (null phaseTasks)
      -> next DeleteTask{ phaseIndex = 0, .. }
    | pressedChar 'l' -> Just <$> assignTasks phase
    | pressedChar 'v' -> do
      now <- Time.getCurrentTime
      next Voting
        { phasePlayersYes = []
        , phasePlayersNo  = []
        , phaseTimeStart  = now
        , phaseTimeNow    = now
        , phaseVoteLength = 10 -- seconds
        , ..
        }
    | pressedChar ' ' -> do
      now <- Time.getCurrentTime
      next Inspection
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
    | otherwise -> next phase
  ConfirmQuit{..}
    | pressedChar 'y' -> return Nothing
    | pressedChar 'n' -> next Waiting{..}
    | otherwise       -> next phase
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
  AddPlayerName{..} -> next $ let
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
      | pressedKey Vty.KEnter ->
        Waiting{ phasePlayers = phasePlayers ++ [newPlayer], .. }
      | pressedKey Vty.KEsc -> AddPlayerNo{..}
      | otherwise -> AddPlayerName{ phaseName = name', .. }
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
  AddTask{..} -> next $ let
    funs = flip map keys $ \case
      Vty.KBS     -> \s -> take (length s - 1) s
      Vty.KChar c -> (++ [c])
      _           -> id
    task' = foldl (flip ($)) phaseNewTask funs
    in if
      | pressedKey Vty.KEnter -> Waiting{ phaseTasks = phaseTasks ++ [(task', [])], .. }
      | pressedKey Vty.KEsc   -> Waiting{..}
      | otherwise             -> AddTask{ phaseNewTask = task', .. }
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
    now <- Time.getCurrentTime
    let undecided = do
          (playerIndex, player) <- zip [0..] phasePlayers
          guard $ notElem playerIndex $ phasePlayersYes ++ phasePlayersNo
          return (playerIndex, player)
        newYes = phasePlayersYes ++
          [ ix | (ix, player) <- undecided, pressedYes player ]
        newNo = phasePlayersNo ++
          [ ix | (ix, player) <- undecided, pressedNo player, ix `notElem` newYes ]
    next $ if
      | pressedKey Vty.KEsc -> Waiting{..}
      | Time.diffUTCTime now phaseTimeStart >= phaseVoteLength -- time's up
        || length newYes + length newNo == length phasePlayers -- everyone's voted
        || pressedChar 'v'                                     -- ended vote early
        -> VoteComplete
          { phasePlayersYes = newYes
          , phasePlayersNo  = newNo
          , ..
          }
      | otherwise -> Voting
        { phaseTimeNow    = now
        , phasePlayersYes = newYes
        , phasePlayersNo  = newNo
        , ..
        }
  VoteComplete{..} -> next $ if
    | pressedKey Vty.KEsc -> Waiting{..}
    | otherwise           -> phase
  Inspection{..} -> do
    now <- Time.getCurrentTime
    let undecided = do
          (playerIndex, player) <- zip [0..] phasePlayers
          guard $ notElem playerIndex $ map fst $ phasePlayersGood ++ phasePlayersBad
          return (playerIndex, player)
        newGood = phasePlayersGood ++
          [ (ix, now) | (ix, player) <- undecided, pressedYes player ]
        newBad = phasePlayersBad ++
          [ (ix, now) | (ix, player) <- undecided, pressedNo player, ix `notElem` map fst newGood ]
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
  Citation{..} -> next $ if
    | pressedKey Vty.KUp    ->
      Citation{ phaseIndex = max 0 $ phaseIndex - 1, .. }
    | pressedKey Vty.KDown  ->
      Citation{ phaseIndex = min (length phasePlayers - 1) $ phaseIndex + 1, .. }
    | pressedKey Vty.KEsc   -> Waiting{..}
    | pressedKey Vty.KEnter -> Waiting
      { phasePlayers = case splitAt phaseIndex phasePlayers of
        (xs, player : ys) -> let
          cited = player{ playerCitations = playerCitations player + 1 }
          in xs ++ [cited] ++ ys
        (_ , []         ) -> phasePlayers -- shouldn't happen
      , ..
      }
    | otherwise -> phase
  where
    next = return . Just
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
