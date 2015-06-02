{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.Time.Clock

data Button360
  -- buttons, from 0 to 14
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
  | Dpad Ortho -- UDLR are 11,12,13,14
  -- axes
  | LTrigger -- axis 2. minBound=normal, maxBound=pressed
  | RTrigger -- axis 5. minBound=normal, maxBound=pressed
  | LStick Ortho -- X is axis 0, Y is 1. minBound is U/L, maxBound is D/R
  | RStick Ortho -- X is axis 3, Y is 4. minBound is U/L, maxBound is D/R
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
code :: (Eq a, Num a, MonadIO m) => a -> m a -> m ()
code c act = do
  n <- act
  unless (n == c) $ SDL.getError >>= liftIO . peekCString >>= error

withSDL :: [SDL.InitFlag] -> IO a -> IO a
withSDL flags = bracket_
  (code 0 $ SDL.init $ foldr (.|.) 0 flags)
  SDL.quit

data Phase
  = Waiting
    { phasePlayers :: [Player]
    , phaseTasks :: [(Task, [Int])]
    }
  | AddPlayerYes
    { phasePlayers :: [Player]
    , phaseTasks :: [(Task, [Int])]
    }
  | AddPlayerNo
    { phaseJoystick :: SDL.JoystickID
    , phaseYes :: Button360
    , phasePlayers :: [Player]
    , phaseTasks :: [(Task, [Int])]
    }
  | AddPlayerName
    { phaseNewPlayer :: Player
    , phasePlayers :: [Player]
    , phaseTasks :: [(Task, [Int])]
    }
  | DeletePlayer
    { phaseIndex :: Int
    , phasePlayers :: [Player]
    , phaseTasks :: [(Task, [Int])]
    }
  | AddTask
    { phaseNewTask :: Task
    , phasePlayers :: [Player]
    , phaseTasks :: [(Task, [Int])]
    }
  | DeleteTask
    { phaseIndex :: Int
    , phasePlayers :: [Player]
    , phaseTasks :: [(Task, [Int])]
    }
  | Voting
    { phasePlayersYes  :: [Int]
    , phasePlayersNo   :: [Int]
    , phaseVoteStart   :: UTCTime
    , phaseCurrentTime :: UTCTime
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
  deriving (Eq, Ord, Show)

data Player = Player
  { playerName :: String
  , playerJoystick :: SDL.JoystickID
  , playerYes :: Button360
  , playerNo :: Button360
  } deriving (Eq, Ord, Show, Read)

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

update :: [(SDL.JoystickID, Button360)] -> [Vty.Key] -> Phase -> IO (Maybe Phase)
update sdl keys phase = case phase of
  Waiting{..}
    | pressedChar 'p' -> next $ AddPlayerYes{..}
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
        , phasePlayersNo = []
        , phaseVoteStart = now
        , phaseCurrentTime = now
        , phaseVoteLength = 10 -- seconds
        , ..
        }
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
      buttonNo : _ -> AddPlayerName
        { phaseNewPlayer = Player
          { playerName     = ""
          , playerJoystick = phaseJoystick
          , playerYes      = phaseYes
          , playerNo       = buttonNo
          }
        , ..
        }
      []  | pressedKey Vty.KEsc -> AddPlayerYes{..}
          | otherwise           -> phase
  AddPlayerName{..} -> next $ let
    funs = flip map keys $ \case
      Vty.KBS     -> \s -> take (length s - 1) s
      Vty.KChar c -> (++ [c])
      _           -> id
    name' = take 20 $ foldl (flip ($)) (playerName phaseNewPlayer) funs
    updatedPlayer = phaseNewPlayer{ playerName = name' }
    in if
      | pressedKey Vty.KEnter ->
        Waiting{ phasePlayers = phasePlayers ++ [updatedPlayer], .. }
      | pressedKey Vty.KEsc -> AddPlayerNo
        { phaseJoystick = playerJoystick phaseNewPlayer
        , phaseYes = playerYes phaseNewPlayer
        , ..
        }
      | otherwise -> AddPlayerName{ phaseNewPlayer = updatedPlayer, .. }
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
    task' = take 30 $ foldl (flip ($)) phaseNewTask funs
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
        newYes = phasePlayersYes ++ do
          (playerIndex, Player{..}) <- undecided
          (joy, btn) <- sdl
          guard $ joy == playerJoystick && btn == playerYes
          return playerIndex
        newNo = phasePlayersNo ++ do
          (playerIndex, Player{..}) <- undecided
          (joy, btn) <- sdl
          guard $ joy == playerJoystick && btn == playerNo
          return playerIndex
    next $ if
      | pressedKey Vty.KEsc -> Waiting{..}
      | diffUTCTime now phaseVoteStart >= phaseVoteLength
        || length newYes + length newNo == length phasePlayers
        -> VoteComplete
          { phasePlayersYes = newYes
          , phasePlayersNo  = newNo
          , ..
          }
      | otherwise -> Voting
        { phaseCurrentTime = now
        , phasePlayersYes = newYes
        , phasePlayersNo = newNo
        , ..
        }
  VoteComplete{..} -> next $ if
    | pressedKey Vty.KEsc -> Waiting{..}
    | otherwise           -> phase
  where
    next = return . Just
    pressedChar c = elem (Vty.KChar c) keys
    pressedKey k = elem k keys
    inUse (joy, btn) = flip any (phasePlayers phase) $ \Player{..} ->
      playerJoystick == joy && elem btn [playerYes, playerNo]

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
    , Vty.string Vty.defAttr "Adding new inspector. Press YES button"
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
  AddPlayerName{ phaseNewPlayer = Player{..}, .. } -> Vty.vertCat
    [ playersAndTasks
    , Vty.string Vty.defAttr ""
    , Vty.horizCat
      [ Vty.string Vty.defAttr $ "Joystick " ++ show playerJoystick ++ ", "
      , Vty.string (color playerJoystick playerYes) $ show playerYes
      , Vty.string Vty.defAttr " for YES, "
      , Vty.string (color playerJoystick playerNo) $ show playerNo
      , Vty.string Vty.defAttr " for NO. Enter name"
      ]
    , Vty.string Vty.defAttr ""
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.cyan `Vty.withForeColor` Vty.white)
      $ take 20 $ playerName ++ repeat ' '
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red `Vty.withForeColor` Vty.white)
      $ take 20 $ cyrillicize playerName ++ repeat ' '
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
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.cyan `Vty.withForeColor` Vty.white)
      $ take 30 $ phaseNewTask ++ repeat ' '
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red `Vty.withForeColor` Vty.white)
      $ take 30 $ cyrillicize phaseNewTask ++ repeat ' '
    ]
  DeleteTask{..} -> Vty.vertCat
    [ playersAndTasks
    , Vty.string Vty.defAttr ""
    , Vty.string Vty.defAttr "Remove which task?"
    ]
  Voting{..} -> Vty.vertCat
    [ Vty.string Vty.defAttr $ let
      remaining = phaseVoteLength - diffUTCTime phaseCurrentTime phaseVoteStart
      in "Vote now! " ++ show (realToFrac remaining :: Double) ++ " seconds left"
    , Vty.string Vty.defAttr ""
    , Vty.string Vty.defAttr $ "YEA (" ++ show (length phasePlayersYes) ++ "):"
    , Vty.string Vty.defAttr ""
    , Vty.vertCat $ flip map phasePlayersYes $ \ix ->
        Vty.string Vty.defAttr $ playerName $ phasePlayers !! ix
    , Vty.string Vty.defAttr ""
    , Vty.string Vty.defAttr $ "NAY (" ++ show (length phasePlayersNo) ++ "):"
    , Vty.vertCat $ flip map phasePlayersYes $ \ix ->
        Vty.string Vty.defAttr $ playerName $ phasePlayers !! ix
    ]
  VoteComplete{..} -> Vty.vertCat
    [ Vty.string Vty.defAttr $ "Voting is over."
    , Vty.string Vty.defAttr ""
    , Vty.string Vty.defAttr $ "YEA (" ++ show (length phasePlayersYes) ++ "):"
    , Vty.string Vty.defAttr ""
    , Vty.vertCat $ flip map phasePlayersYes $ \ix ->
        Vty.string Vty.defAttr $ playerName $ phasePlayers !! ix
    , Vty.string Vty.defAttr ""
    , Vty.string Vty.defAttr $ "NAY (" ++ show (length phasePlayersNo) ++ "):"
    , Vty.vertCat $ flip map phasePlayersYes $ \ix ->
        Vty.string Vty.defAttr $ playerName $ phasePlayers !! ix
    ]
  where
    playersAndTasks = Vty.vertCat
      [ Vty.string Vty.defAttr "Inspectors:"
      , imagePlayersTasks
      , Vty.string Vty.defAttr ""
      , Vty.string Vty.defAttr "Tasks:"
      , imageTasks
      ]
    imagePlayersTasks = Vty.vertCat $ map imagePlayerTasks $ getPlayersTasks phase
    imagePlayerTasks (b, player, tasks) = Vty.horizCat
      [ Vty.string Vty.defAttr $ if b then "* " else "  "
      , Vty.vertCat
        [ imagePlayer player
        , Vty.pad 2 0 0 0 $ Vty.vertCat $ map (Vty.string Vty.defAttr) tasks
        ]
      ]
    imagePlayer Player{..} = Vty.horizCat
      [ Vty.string Vty.defAttr $ playerName ++ " (joystick " ++ show playerJoystick ++ ", "
      , Vty.string (color playerJoystick playerYes) $ show playerYes
      , Vty.string Vty.defAttr $ " for yes, "
      , Vty.string (color playerJoystick playerNo) $ show playerNo
      , Vty.string Vty.defAttr " for no)"
      ]
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
        _ -> "  "
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
    _threadid <- forkIO $ forever $ Vty.nextEvent vty >>= putMVar vtyEvent

    let loop :: Phase -> [Set.Set Button360] -> IO ()
        loop phase prev = do
          Vty.update vty $ Vty.picForImage $ draw prev phase
          liftIO $ threadDelay 5000
          sdlEvents <- untilNothing pollSDL
          vtyEvents <- untilNothing $ tryTakeMVar vtyEvent
          let keys = [ k | Vty.EvKey k _ <- vtyEvents ]
              curr = foldr ($) prev $ map modifyButtons sdlEvents
          update (newPresses prev curr) keys phase >>= \case
            Just phase' -> loop phase' curr
            Nothing     -> Vty.shutdown vty

        startState = Waiting{ phasePlayers = [], phaseTasks = [] }
        startButtons = map (const Set.empty) joys

    loop startState startButtons
