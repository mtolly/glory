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
    , phaseButtonYes :: Button360
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
  deriving (Eq, Ord, Show, Read)

data Player = Player
  { playerName :: String
  , playerJoystick :: SDL.JoystickID
  , playerYes :: Button360
  , playerNo :: Button360
  } deriving (Eq, Ord, Show, Read)

type Task = String

-- | not srs, just for funsies
cyrillicize :: String -> String
cyrillicize = let
  str = "АБЦДЕФГХИЖКЛМНОПQРСТУВЪЬЙЗ"
  mapping = zip ['A'..'Z'] str ++ zip ['a'..'z'] (map toLower str)
  in map $ \c -> fromMaybe c $ lookup c mapping

getPlayersTasks :: Phase -> [(Player, [Task])]
getPlayersTasks p = do
  (ix, player) <- zip [0..] $ phasePlayers p
  let tasks = [ task | (task, ixs) <- phaseTasks p, elem ix ixs ]
  return (player, tasks)

newPresses :: [Set.Set Button360] -> [Set.Set Button360] -> [(SDL.JoystickID, Button360)]
newPresses prev curr = concat $ zipWith3 f [0..] prev curr where
  f i set1 set2 = map (i,) $ Set.toList $ Set.difference set2 set1

update :: [(SDL.JoystickID, Button360)] -> [Vty.Key] -> Phase -> Maybe Phase
update sdl keys phase = case phase of
  Waiting{..}
    | pressedChar 'p' -> Just $ AddPlayerYes{..}
    | pressedKey Vty.KEsc -> Nothing
    | pressedKey Vty.KDel && not (null phasePlayers)
      -> Just $ DeletePlayer{phaseIndex = 0, ..}
    | otherwise -> Just phase
  AddPlayerYes{..} -> Just $ case sdl of
    (phaseJoystick, phaseButtonYes) : _ -> AddPlayerNo{..}
    [] -> if pressedChar 'q' then Waiting{..} else phase
  AddPlayerNo{..} -> Just $ case [ btnNo | (joy', btnNo) <- sdl, phaseJoystick == joy' ] of
    buttonNo : _ -> AddPlayerName
      { phaseNewPlayer = Player
        { playerName     = ""
        , playerJoystick = phaseJoystick
        , playerYes      = phaseButtonYes
        , playerNo       = buttonNo
        }
      , ..
      }
    [] -> if pressedChar 'q' then AddPlayerYes{..} else phase
  AddPlayerName{..} -> Just $ let
    funs = flip map keys $ \case
      Vty.KBS     -> \s -> take (length s - 1) s
      Vty.KChar c -> (++ [c])
      _           -> id
    name' = take 20 $ foldl (flip ($)) (playerName phaseNewPlayer) funs
    updatedPlayer = phaseNewPlayer{ playerName = name' }
    in if
      | null keys             -> phase
      | pressedKey Vty.KEnter -> Waiting
        { phasePlayers = phasePlayers ++ [updatedPlayer]
        , ..
        }
      | pressedChar 'q' -> AddPlayerNo
        { phaseJoystick = playerJoystick phaseNewPlayer
        , phaseButtonYes = playerYes phaseNewPlayer
        , ..
        }
      | otherwise             -> AddPlayerName
        { phaseNewPlayer = updatedPlayer
        , ..
        }
  DeletePlayer{..} -> Just $ if
    | pressedKey Vty.KUp -> DeletePlayer
      { phaseIndex = max 0 $ phaseIndex - 1
      , ..
      }
    | pressedKey Vty.KDown -> DeletePlayer
      { phaseIndex = min (length phasePlayers - 1) $ phaseIndex + 1
      , ..
      }
    | pressedChar 'q' -> Waiting{..}
    | pressedKey Vty.KEnter -> Waiting
      { phasePlayers = case splitAt phaseIndex phasePlayers of
        (xs, ys) -> xs ++ drop 1 ys
      , ..
      }
    | otherwise -> phase
  AddTask{} -> undefined
  DeleteTask{} -> undefined
  where pressedChar c = elem (Vty.KChar c) keys
        pressedKey k = elem k keys

untilNothing :: (Monad m) => m (Maybe a) -> m [a]
untilNothing act = act >>= \case
  Just x  -> liftM (x :) $ untilNothing act
  Nothing -> return []

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
          Vty.update vty $ Vty.picForImage $ Vty.string Vty.defAttr $ show phase
          liftIO $ threadDelay 5000
          sdlEvents <- untilNothing pollSDL
          vtyEvents <- untilNothing $ tryTakeMVar vtyEvent
          let keys = [ k | Vty.EvKey k _ <- vtyEvents ]
              curr = foldr ($) prev $ map modifyButtons sdlEvents
          case update (newPresses prev curr) keys phase of
            Nothing     -> return ()
            Just phase' -> loop phase' curr
        startState = Waiting{ phasePlayers = [], phaseTasks = [] }
        startButtons = map (const Set.empty) joys
    loop startState startButtons
