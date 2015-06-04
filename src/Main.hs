{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Control.Exception (bracket_)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.IO (hIsTerminalDevice, stdout)
import Data.Char (toLower)
import qualified Data.Set as Set
import qualified Graphics.Vty as Vty
import System.Random.Shuffle (shuffleM)
import System.Random (randomRIO)
import Data.Time.Clock
import Data.Fixed
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP
import qualified Data.Text as T

data Button360
  = A
  | B
  | X
  | Y
  | LB
  | RB
  | LClick
  | RClick
  | Start
  | Back
  | Xbox
  | Dpad Ortho
  | LTrigger
  | RTrigger
  | LStick Ortho
  | RStick Ortho
  deriving (Eq, Ord, Show, Read)

data Ortho = U | D | L | R
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Returns Just an event if there is one currently in the queue.
pollSDL :: (MonadIO m) => m (Maybe SDL.Event)
pollSDL = liftIO $ alloca $ \pevt -> SDL.pollEvent pevt >>= \case
  1 -> fmap Just $ peek pevt
  _ -> return Nothing

modifyButtons :: SDL.Event -> [Set.Set Button360] -> [Set.Set Button360]
modifyButtons = \case
  SDL.JoyButtonEvent
    { SDL.joyButtonEventButton = btn
    , SDL.joyButtonEventState = SDL.SDL_PRESSED
    , SDL.joyButtonEventWhich = joy
    } -> button joy btn True
  SDL.JoyButtonEvent
    { SDL.joyButtonEventButton = btn
    , SDL.joyButtonEventState = SDL.SDL_RELEASED
    , SDL.joyButtonEventWhich = joy
    } -> button joy btn False
  SDL.JoyAxisEvent
    { SDL.joyAxisEventAxis = n
    , SDL.joyAxisEventValue = v
    , SDL.joyAxisEventWhich = joy
    } -> axis joy n v
  _ -> id
  where
    button joy btn bool = modify (fromIntegral joy) $ case btn of
      0  -> modifier A
      1  -> modifier B
      2  -> modifier X
      3  -> modifier Y
      4  -> modifier LB
      5  -> modifier RB
      6  -> modifier LClick
      7  -> modifier RClick
      8  -> modifier Start
      9  -> modifier Back
      10 -> modifier Xbox
      11 -> modifier $ Dpad U
      12 -> modifier $ Dpad D
      13 -> modifier $ Dpad L
      14 -> modifier $ Dpad R
      _ -> id
      where modifier = if bool then Set.insert else Set.delete
    axis joy n v = modify (fromIntegral joy) $ case n of
      0 -> stick LStick L R
      1 -> stick LStick U D
      2 -> trigger LTrigger
      3 -> stick RStick L R
      4 -> stick RStick U D
      5 -> trigger RTrigger
      _ -> id
      where stick s dmin dmax = if
              | v < (-0x4000) -> Set.insert (s dmin) . Set.delete (s dmax)
              | v > 0x4000    -> Set.delete (s dmin) . Set.insert (s dmax)
              | otherwise     -> Set.delete (s dmin) . Set.delete (s dmax)
            trigger t = if
              | v > 0     -> Set.insert t
              | otherwise -> Set.delete t
    modify i f xs = case splitAt i xs of
      (_ , []    ) -> xs
      (ys, z : zs) -> ys ++ [f z] ++ zs

-- | Extracts and throws an SDL error if the action returns a null pointer.
notNull :: (MonadIO m) => m (Ptr a) -> m (Ptr a)
notNull act = do
  p <- act
  if p == nullPtr
    then SDL.getError >>= liftIO . peekCString >>= error
    else return p

-- | Extracts and throws an SDL error if the action doesn't return the right number.
sdlCode :: (Eq a, Num a, MonadIO m) => a -> m a -> m ()
sdlCode c act = do
  n <- act
  unless (n == c) $ SDL.getError >>= liftIO . peekCString >>= error

withSDL :: [SDL.InitFlag] -> IO a -> IO a
withSDL flags = bracket_
  (sdlCode 0 $ SDL.init $ foldr (.|.) 0 flags)
  SDL.quit

data Phase
  = Waiting
    { phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | AddPlayerYes
    { phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | AddPlayerNo
    { phaseJoystick    :: SDL.JoystickID
    , phaseYes         :: Button360
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | AddPlayerName
    { phaseJoystick    :: SDL.JoystickID
    , phaseYes         :: Button360
    , phaseNo          :: Button360
    , phaseName        :: String
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | AddPlayerAPI
    { phaseName        :: String
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | DeletePlayer
    { phaseIndex       :: Int
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | AddTask
    { phaseNewTask     :: Task
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | DeleteTask
    { phaseIndex       :: Int
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | Voting
    { phasePlayersYes  :: [Int]
    , phasePlayersNo   :: [Int]
    , phaseTimeStart   :: UTCTime
    , phaseTimeNow     :: UTCTime
    , phaseVoteLength  :: NominalDiffTime
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | VoteComplete
    { phasePlayersYes  :: [Int]
    , phasePlayersNo   :: [Int]
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | Inspection
    { phasePlayersGood :: [(Int, UTCTime)]
    , phasePlayersBad  :: [(Int, UTCTime)]
    , phaseTimeStart   :: UTCTime
    , phaseTimeNow     :: UTCTime
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | ChosenOne
    { phaseIndex       :: Int
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  deriving (Eq, Ord, Show)

data Player
  = PlayerJoy
    { playerName :: String
    , playerJoystick :: SDL.JoystickID
    , playerYes :: Button360
    , playerNo :: Button360
    }
  | PlayerAPI
    { playerName :: String
    , playerCode :: String
    }
  deriving (Eq, Ord, Show, Read)

type Task = String

-- | not srs, just sufficient
cyrillicize :: String -> String
cyrillicize = let
  str = "АБЦДЕФГХИЖКЛМНОПQРСТУВЪЬЙЗ"
  mapping = zip ['A'..'Z'] str ++ zip ['a'..'z'] (map toLower str)
  in map $ \c -> fromMaybe c $ lookup c mapping

getPlayersTasks :: Phase -> [(Bool, Player, [Task])]
getPlayersTasks p = do
  let selected = case p of
        DeletePlayer{..} -> Just phaseIndex
        _                -> Nothing
  (ix, player) <- zip [0..] $ phasePlayers p
  let tasks = [ task | (task, ixs) <- phaseTasks p, elem ix ixs ]
  return (selected == Just ix, player, tasks)

newPresses :: [Set.Set Button360] -> [Set.Set Button360] -> [(SDL.JoystickID, Button360)]
newPresses prev curr = concat $ zipWith3 f [0..] prev curr where
  f i set1 set2 = map (i,) $ Set.toList $ Set.difference set2 set1

-- | Assign tasks randomly so each player has at least one task,
-- and each task has at least one player.
assignTasks :: Phase -> IO Phase
assignTasks p = do
  tasks   <- shuffleM $ map fst             $ phaseTasks   p
  players <- shuffleM $ zipWith const [0..] $ phasePlayers p
  let assignment = if length tasks < length players
        then zip (cycle tasks) players
        else zip tasks (cycle players)
      indexesFor task = [ i | (t, i) <- assignment, t == task ]
      newMapping = [ (t, indexesFor t) | (t, _) <- phaseTasks p ]
  return $ p { phaseTasks = newMapping }

update :: [(SDL.JoystickID, Button360)] -> [Vty.Key] -> [(String, Bool)] -> Phase -> IO (Maybe Phase)
update sdl keys api phase = case phase of
  Waiting{..}
    | pressedChar '3' -> next $ AddPlayerYes{..}
    | pressedChar 'a' -> next $ AddPlayerAPI{phaseName = "", ..}
    | pressedChar '=' -> next $ AddTask{phaseNewTask = "", ..}
    | pressedKey Vty.KEsc -> return Nothing
    | pressedKey Vty.KDel && not (null phasePlayers)
      -> next $ DeletePlayer{phaseIndex = 0, ..}
    | pressedChar '-' && not (null phaseTasks)
      -> next $ DeleteTask{phaseIndex = 0, ..}
    | pressedChar 'l' -> fmap Just $ assignTasks phase
    | pressedChar 'v' -> do
      now <- getCurrentTime
      next $ Voting
        { phasePlayersYes = []
        , phasePlayersNo  = []
        , phaseTimeStart  = now
        , phaseTimeNow    = now
        , phaseVoteLength = 10 -- seconds
        , ..
        }
    | pressedChar ' ' -> do
      now <- getCurrentTime
      next $ Inspection
        { phasePlayersGood = []
        , phasePlayersBad  = []
        , phaseTimeStart   = now
        , phaseTimeNow     = now
        , ..
        }
    | pressedChar '1' && not (null phasePlayers) -> do
      phaseIndex <- randomRIO (0, length phasePlayers - 1)
      next ChosenOne{..}
    | otherwise -> next phase
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
      { playerName     = name'
      , playerJoystick = phaseJoystick
      , playerYes      = phaseYes
      , playerNo       = phaseNo
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
      return $ PlayerAPI
        { playerName = name'
        , playerCode = code
        }
    usedCodes = do
      PlayerAPI{..} <- phasePlayers
      return $ playerCode
    newCode = do
      code <- replicateM 4 $ randomRIO ('A', 'Z')
      if elem code usedCodes
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
    now <- getCurrentTime
    let undecided = do
          (playerIndex, player) <- zip [0..] phasePlayers
          guard $ not $ elem playerIndex $ phasePlayersYes ++ phasePlayersNo
          return (playerIndex, player)
        newYes = phasePlayersYes
          ++ do
            (playerIndex, PlayerJoy{..}) <- undecided
            (joy, btn) <- sdl
            guard $ joy == playerJoystick && btn == playerYes
            return playerIndex
          ++ do
            (playerIndex, PlayerAPI{..}) <- undecided
            (code, True) <- api
            guard $ code == playerCode
            return playerIndex
        newNo = filter (\i -> not $ elem i newYes) $ phasePlayersNo
          ++ do
            (playerIndex, PlayerJoy{..}) <- undecided
            (joy, btn) <- sdl
            guard $ joy == playerJoystick && btn == playerNo
            return playerIndex
          ++ do
            (playerIndex, PlayerAPI{..}) <- undecided
            (code, False) <- api
            guard $ code == playerCode
            return playerIndex
    next $ if
      | pressedKey Vty.KEsc -> Waiting{..}
      | diffUTCTime now phaseTimeStart >= phaseVoteLength -- time's up
        || length newYes + length newNo == length phasePlayers -- everyone's voted
        || pressedChar 'v'
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
    now <- getCurrentTime
    let undecided = do
          (playerIndex, player) <- zip [0..] phasePlayers
          guard $ not $ elem playerIndex $ map fst $ phasePlayersGood ++ phasePlayersBad
          return (playerIndex, player)
        newGood = phasePlayersGood
          ++ do
            (playerIndex, PlayerJoy{..}) <- undecided
            (joy, btn) <- sdl
            guard $ joy == playerJoystick && btn == playerYes
            return (playerIndex, now)
          ++ do
            (playerIndex, PlayerAPI{..}) <- undecided
            (code, True) <- api
            guard $ code == playerCode
            return (playerIndex, now)
        newBad = filter (\(i, _) -> not $ elem i $ map fst newGood) $ phasePlayersBad
          ++ do
            (playerIndex, PlayerJoy{..}) <- undecided
            (joy, btn) <- sdl
            guard $ joy == playerJoystick && btn == playerNo
            return (playerIndex, now)
          ++ do
            (playerIndex, PlayerAPI{..}) <- undecided
            (code, False) <- api
            guard $ code == playerCode
            return (playerIndex, now)
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
  where
    next = return . Just
    pressedChar c = elem (Vty.KChar c) keys
    pressedKey k = elem k keys
    inUse (joy, btn) = flip any (phasePlayers phase) $ \case
      PlayerJoy{..} -> playerJoystick == joy && elem btn [playerYes, playerNo]
      PlayerAPI{}   -> False

untilNothing :: (Monad m) => m (Maybe a) -> m [a]
untilNothing act = act >>= \case
  Just x  -> liftM (x :) $ untilNothing act
  Nothing -> return []

draw :: [Set.Set Button360] -> Phase -> Vty.Image
draw btns phase = case phase of
  Waiting{..} -> Vty.vertCat
    [ playersAndTasks
    , Vty.pad 0 1 0 0 $ Vty.string Vty.defAttr $ case length phasePlayers of
      1 -> "1 inspector ready."
      n -> show n ++ " inspectors ready."
    ]
  AddPlayerYes{..} -> Vty.vertCat
    [ playersAndTasks
    , Vty.string Vty.defAttr ""
    , Vty.string Vty.defAttr "Adding new controller inspector. Press YES button"
    ]
  AddPlayerNo{..} -> Vty.vertCat
    [ playersAndTasks
    , Vty.string Vty.defAttr ""
    , Vty.horizCat
      [ Vty.string Vty.defAttr $ "Joystick " ++ show phaseJoystick ++ ", "
      , Vty.string (color phaseJoystick phaseYes) $ show phaseYes
      , Vty.string Vty.defAttr " for YES. Press NO button"
      ]
    ]
  AddPlayerName{..} -> Vty.vertCat
    [ playersAndTasks
    , Vty.string Vty.defAttr ""
    , Vty.horizCat
      [ Vty.string Vty.defAttr $ "Joystick " ++ show phaseJoystick ++ ", "
      , Vty.string (color phaseJoystick phaseYes) $ show phaseYes
      , Vty.string Vty.defAttr " for YES, "
      , Vty.string (color phaseJoystick phaseNo) $ show phaseNo
      , Vty.string Vty.defAttr " for NO. Enter name"
      ]
    , Vty.string Vty.defAttr ""
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.cyan `Vty.withForeColor` Vty.white) phaseName
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red `Vty.withForeColor` Vty.white) $ cyrillicize phaseName
    , Vty.string Vty.defAttr "" -- reset color
    ]
  AddPlayerAPI{..} -> Vty.vertCat
    [ playersAndTasks
    , Vty.string Vty.defAttr ""
    , Vty.string Vty.defAttr "Adding new API inspector. Enter name"
    , Vty.string Vty.defAttr ""
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.cyan `Vty.withForeColor` Vty.white) phaseName
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red `Vty.withForeColor` Vty.white) $ cyrillicize phaseName
    , Vty.string Vty.defAttr "" -- reset color
    ]
  DeletePlayer{..} -> Vty.vertCat
    [ playersAndTasks
    , Vty.string Vty.defAttr ""
    , Vty.string Vty.defAttr "Remove which inspector from duty?"
    ]
  AddTask{..} -> Vty.vertCat
    [ playersAndTasks
    , Vty.string Vty.defAttr ""
    , Vty.string Vty.defAttr "Enter new task name"
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.cyan `Vty.withForeColor` Vty.white) phaseNewTask
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red `Vty.withForeColor` Vty.white) $ cyrillicize phaseNewTask
    , Vty.string Vty.defAttr "" -- reset color
    ]
  DeleteTask{..} -> Vty.vertCat
    [ playersAndTasks
    , Vty.string Vty.defAttr ""
    , Vty.string Vty.defAttr "Remove which task?"
    ]
  Voting{..} -> Vty.vertCat
    [ Vty.string Vty.defAttr $ let
      remaining = phaseVoteLength - diffUTCTime phaseTimeNow phaseTimeStart
      in "Vote now! " ++ showTime remaining ++ " seconds left"
    , Vty.string Vty.defAttr ""
    , Vty.string (Vty.defAttr `Vty.withForeColor` Vty.green) $ "YEA (" ++ show (length phasePlayersYes) ++ "):"
    , Vty.vertCat $ flip map phasePlayersYes $ \ix ->
        Vty.string Vty.defAttr $ "  " ++ playerName (phasePlayers !! ix)
    , Vty.string Vty.defAttr ""
    , Vty.string (Vty.defAttr `Vty.withForeColor` Vty.red) $ "NAY (" ++ show (length phasePlayersNo) ++ "):"
    , Vty.vertCat $ flip map phasePlayersNo $ \ix ->
        Vty.string Vty.defAttr $ "  " ++ playerName (phasePlayers !! ix)
    ]
  VoteComplete{..} -> Vty.vertCat
    [ Vty.string Vty.defAttr $ "Voting is over."
    , Vty.string Vty.defAttr ""
    , Vty.string (Vty.defAttr `Vty.withForeColor` Vty.green) $ "YEA (" ++ show (length phasePlayersYes) ++ "):"
    , Vty.vertCat $ flip map phasePlayersYes $ \ix ->
        Vty.string Vty.defAttr $ "  " ++ playerName (phasePlayers !! ix)
    , Vty.string Vty.defAttr ""
    , Vty.string (Vty.defAttr `Vty.withForeColor` Vty.red) $ "NAY (" ++ show (length phasePlayersNo) ++ "):"
    , Vty.vertCat $ flip map phasePlayersNo $ \ix ->
        Vty.string Vty.defAttr $ "  " ++ playerName (phasePlayers !! ix)
    ]
  Inspection{..} -> Vty.vertCat
    [ Vty.string Vty.defAttr $ if inspectionDone then "Inspection complete." else "Inspection is underway."
    , Vty.string Vty.defAttr $ "Time: " ++ if inspectionDone
      then let
        allTimes = [ t | (_, t) <- phasePlayersGood ++ phasePlayersBad ]
        in showTime $ diffUTCTime (foldr max phaseTimeStart allTimes) phaseTimeStart
      else showTime $ diffUTCTime phaseTimeNow phaseTimeStart
    , Vty.string Vty.defAttr ""
    , Vty.vertCat $ flip map (zip [0..] phasePlayers) $ \(i, player) -> let
        good = [ time | (j, time) <- phasePlayersGood, i == j ]
        bad  = [ time | (j, time) <- phasePlayersBad , i == j ]
        tasks = [ task | (task, ixs) <- phaseTasks, elem i ixs ]
        attr = case (good, bad) of
          ([]   , []   ) -> Vty.defAttr
          (_ : _, _    ) -> Vty.defAttr `Vty.withForeColor` Vty.green
          (_    , _ : _) -> Vty.defAttr `Vty.withForeColor` Vty.white `Vty.withBackColor` Vty.red
        in Vty.vertCat
          [ Vty.string attr $ case good ++ bad of
            time : _ -> playerName player ++ " (" ++ showTime (diffUTCTime time phaseTimeStart) ++ ")"
            []       -> playerName player
          , Vty.vertCat [ Vty.string attr $ "  " ++ task | task <- tasks ]
          ]
    , Vty.string Vty.defAttr "" -- reset color
    ] where inspectionDone = length (phasePlayersGood ++ phasePlayersBad) == length phasePlayers
  ChosenOne{..} -> Vty.vertCat
    [ Vty.string Vty.defAttr "A player has been chosen!"
    , Vty.string Vty.defAttr ""
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red `Vty.withForeColor` Vty.white) $
      "  " ++ playerName (phasePlayers !! phaseIndex)
    , Vty.string Vty.defAttr "" -- color reset
    ]
  where
    showTime :: NominalDiffTime -> String
    showTime t = show (realToFrac t :: Milli)
    playersAndTasks = Vty.vertCat
      [ Vty.string Vty.defAttr "Inspectors:"
      , imagePlayersTasks
      , Vty.string Vty.defAttr ""
      , Vty.string Vty.defAttr "Tasks:"
      , imageTasks
      ]
    imagePlayersTasks = Vty.vertCat $ zipWith imagePlayerTasks [0..] $ getPlayersTasks phase
    imagePlayerTasks i (b, player, tasks) = Vty.horizCat
      [ Vty.string Vty.defAttr $ if b then "* " else "  "
      , Vty.vertCat
        [ imagePlayer i player
        , Vty.pad 2 0 0 0 $ Vty.vertCat $ map (Vty.string Vty.defAttr) tasks
        ]
      ]
    imagePlayer :: Int -> Player -> Vty.Image
    imagePlayer i PlayerJoy{..} = Vty.horizCat
      [ Vty.string (Vty.defAttr `Vty.withForeColor` nameColor i) playerName
      , Vty.string Vty.defAttr $ " (joystick " ++ show playerJoystick ++ ", "
      , Vty.string (color playerJoystick playerYes) $ show playerYes
      , Vty.string Vty.defAttr $ " for yes, "
      , Vty.string (color playerJoystick playerNo) $ show playerNo
      , Vty.string Vty.defAttr " for no)"
      ]
    imagePlayer i PlayerAPI{..} = Vty.horizCat
      [ Vty.string (Vty.defAttr `Vty.withForeColor` nameColor i) playerName
      , Vty.string Vty.defAttr $ " (API code " ++ playerCode ++ ")"
      ]
    nameColor :: Int -> Vty.Color
    nameColor i = case i of
      0 -> rgb 234 60 60
      1 -> rgb 239 160 40
      2 -> rgb 226 226 59
      3 -> rgb 88 219 65
      4 -> rgb 48 232 232
      5 -> rgb 49 111 234
      6 -> rgb 148 78 229
      7 -> rgb 239 95 239
      _ -> rgb 170 132 99
      where rgb :: Int -> Int -> Int -> Vty.Color
            rgb = Vty.rgbColor
    color joy btn = if Set.member btn $ btns !! fromIntegral joy
      then Vty.defAttr `Vty.withForeColor` case btn of
        A -> Vty.green
        B -> Vty.red
        X -> Vty.blue
        Y -> Vty.yellow
        _ -> Vty.magenta
      else Vty.defAttr
    imageTasks = Vty.vertCat $ zipWith taskLine [0..] $ map fst $ phaseTasks phase
    taskLine i task = Vty.horizCat
      [ Vty.string Vty.defAttr $ case phase of
          DeleteTask{..} | phaseIndex == i -> "* "
          _                                -> "  "
      , Vty.string Vty.defAttr task
      ]

main :: IO ()
main = do
  b <- hIsTerminalDevice stdout
  unless b $ error "Try again comrade. TTY is required"
  withSDL [SDL.SDL_INIT_JOYSTICK] $ do

    njoy <- SDL.numJoysticks
    joys <- forM [0 .. njoy - 1] $ notNull . SDL.joystickOpen

    vty <- Vty.standardIOConfig >>= Vty.mkVty
    vtyEvent <- newEmptyMVar
    _ <- forkIO $ forever $ Vty.nextEvent vty >>= putMVar vtyEvent

    apiEvent <- newEmptyMVar
    _ <- forkIO $ Warp.run 7081 $ \req f ->
      case map T.toUpper $ filter (not . T.null) $ Wai.pathInfo req of
        [code, "YES"] -> do
          putMVar apiEvent (T.unpack code, True)
          f $ Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")] "Received YES."
        [code, "NO"] -> do
          putMVar apiEvent (T.unpack code, False)
          f $ Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")] "Received NO."
        _ -> do
          f $ Wai.responseLBS HTTP.status400 [(HTTP.hContentType, "text/plain")] "Invalid request."

    let loop :: Phase -> [Set.Set Button360] -> IO ()
        loop phase prev = do
          Vty.update vty $ Vty.picForImage $ draw prev phase
          liftIO $ threadDelay 5000
          sdlEvents <- untilNothing pollSDL
          vtyEvents <- untilNothing $ tryTakeMVar vtyEvent
          apiEvents <- untilNothing $ tryTakeMVar apiEvent
          let keys = [ k | Vty.EvKey k _ <- vtyEvents ]
              curr = foldr ($) prev $ map modifyButtons sdlEvents
          update (newPresses prev curr) keys apiEvents phase >>= \case
            Just phase' -> loop phase' curr
            Nothing     -> Vty.shutdown vty

        startState = Waiting{ phasePlayers = [], phaseTasks = [] }
        startButtons = map (const Set.empty) joys

    loop startState startButtons
