{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Control.Exception (bracket_)
import Control.Concurrent (threadDelay)
import Control.Monad
import qualified UI.NCurses as NC
import Control.Monad.IO.Class
import Data.Maybe (mapMaybe, fromMaybe)
import System.IO (hIsTerminalDevice, stdout)
import Data.Char (toLower)

-- | Returns Just an event if there is one currently in the queue.
pollSDL :: (MonadIO m) => m (Maybe SDL.Event)
pollSDL = liftIO $ alloca $ \pevt -> SDL.pollEvent pevt >>= \case
  1 -> fmap Just $ peek pevt
  _ -> return Nothing

untilNothing :: (Monad m) => m (Maybe a) -> m [a]
untilNothing act = act >>= \case
  Just x  -> liftM (x :) $ untilNothing act
  Nothing -> return []

pollAllEvents :: NC.Window -> NC.Curses ([SDL.Event], [NC.Event])
pollAllEvents w = liftM2 (,) (untilNothing pollSDL) (untilNothing $ NC.getEvent w $ Just 0)

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
  | DpadUp
  | DpadDown
  | DpadLeft
  | DpadRight
  -- axes
  | LT
  | RT
  -- TODO: sticks
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

isButtonPress :: SDL.Event -> Maybe (SDL.JoystickID, Button360)
isButtonPress = \case
  SDL.JoyButtonEvent
    { SDL.joyButtonEventButton = button
    , SDL.joyButtonEventState = SDL.SDL_PRESSED
    , SDL.joyButtonEventWhich = joystick
    } -> Just (joystick, toEnum $ fromIntegral button)
  _ -> Nothing

data Phase
  = Waiting
    { phasePlayers :: [Player]
    , phaseTasks :: [String]
    }
  | MapYes
    { phasePlayers :: [Player]
    , phaseTasks :: [String]
    }
  | MapNo
    { phaseJoystick :: SDL.JoystickID
    , phaseButtonYes :: Button360
    , phasePlayers :: [Player]
    , phaseTasks :: [String]
    }
  | TypeName
    { phaseNewPlayer :: Player
    , phasePlayers :: [Player]
    , phaseTasks :: [String]
    }
  | DeletePlayer
    { phaseIndex :: Int
    , phasePlayers :: [Player]
    , phaseTasks :: [String]
    }
  | AddTask
    { phaseNewTask :: String
    , phasePlayers :: [Player]
    , phaseTasks :: [String]
    }
  | DeleteTask
    { phaseIndex :: Int
    , phasePlayers :: [Player]
    , phaseTasks :: [String]
    }
  deriving (Eq, Ord, Show, Read)

data Player = Player
  { playerName :: String
  , playerJoystick :: SDL.JoystickID
  , playerYes :: Button360
  , playerNo :: Button360
  } deriving (Eq, Ord, Show, Read)

-- | Updates but does not render
clear :: NC.Window -> NC.Curses ()
clear w = do
  (rows, cols) <- NC.screenSize
  NC.updateWindow w $ do
    forM_ [0 .. rows - 1] $ \r -> do
      NC.moveCursor r 0
      NC.drawLineH (Just $ NC.Glyph ' ' []) cols

-- | not srs, just for funsies
cyrillicize :: String -> String
cyrillicize = let
  str = "АБЦДЕФГХИЖКЛМНОПQРСТУВЪЬЙЗ"
  mapping = zip ['A'..'Z'] str ++ zip ['a'..'z'] (map toLower str)
  in map $ \c -> fromMaybe c $ lookup c mapping

-- | Updates and renders a complete state
draw :: NC.Window -> Phase -> NC.Curses ()
draw w p = do
  clear w
  NC.updateWindow w $ do
    NC.drawBox Nothing Nothing
    let players = phasePlayers p
        len = length players
    forM_ (zip [0..] players) $ \(i, player) -> do
      NC.moveCursor (i + 2) 4
      NC.drawString $ unwords
        [ playerName player
        , "(joystick"
        , show (playerJoystick player) ++ ","
        , show $ playerYes player
        , "for yes,"
        , show $ playerNo player
        , "for no)"
        ]
    NC.moveCursor (fromIntegral $ len + 3) 2
    case p of
      Waiting{..} -> do
        NC.drawString $ unwords
          [ show len
          , if len == 1 then "inspector" else "inspectors"
          , "ready."
          ]
      MapYes{..} -> do
        NC.drawString "Adding new inspector. Press YES button"
      MapNo{..} -> do
        NC.drawString $ unwords
          [ "Joystick"
          , show phaseJoystick ++ ","
          , "YES is"
          , show phaseButtonYes ++ ". Press NO button"
          ]
      TypeName{..} -> do
        NC.drawString $ unwords
          [ "Joystick"
          , show (playerJoystick phaseNewPlayer) ++ ","
          , "YES is"
          , show (playerYes phaseNewPlayer) ++ ","
          , "NO is"
          , show (playerNo phaseNewPlayer) ++ "."
          , "Enter name"
          ]
        NC.moveCursor (fromIntegral $ len + 4) 2
        NC.drawString $ playerName phaseNewPlayer
        NC.moveCursor (fromIntegral $ len + 5) 2
        NC.drawString $ cyrillicize $ playerName phaseNewPlayer
      DeletePlayer{..} -> do
        NC.drawString "Remove which inspector from duty?"
        NC.moveCursor (fromIntegral $ phaseIndex + 2) 2
        NC.drawString "*"
      AddTask{} -> undefined
      DeleteTask{} -> undefined
  NC.render

-- | Updates and renders a complete state,
-- but can be optimized given the previous state
drawChange :: NC.Window -> Phase -> Phase -> NC.Curses ()
drawChange w pold pnew
  | pold == pnew = return ()
  | otherwise    = draw w pnew

main :: IO ()
main = do
  b <- hIsTerminalDevice stdout
  if not b
    then error "Try again comrade. TTY is required"
    else withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_JOYSTICK, SDL.SDL_INIT_HAPTIC, SDL.SDL_INIT_EVENTS] $ do
      NC.runCurses $ do
        w <- NC.defaultWindow
        _ <- NC.setCursorMode NC.CursorInvisible
        NC.setEcho False
        njoy <- SDL.numJoysticks
        _joys <- forM [0 .. njoy - 1] $ notNull . SDL.joystickOpen
        nhap <- SDL.numHaptics
        _haps <- forM [0 .. nhap - 1] $ notNull . SDL.hapticOpen
        let loop :: Phase -> NC.Curses ()
            loop phase = do
              liftIO $ threadDelay 5000
              (sdl, cur) <- pollAllEvents w
              let continue phase' = do
                    if any (== NC.EventResized) cur -- TODO: isn't working
                      then draw w phase'
                      else drawChange w phase phase'
                    loop phase'
              case phase of
                Waiting{..}
                  | any (== NC.EventCharacter 'p') cur
                    -> continue $ MapYes{..}
                  | any (== NC.EventCharacter '\ESC') cur
                    -> return () -- press ESC to quit
                  | any (== NC.EventCharacter '\DEL') cur && not (null phasePlayers)
                    -> continue $ DeletePlayer{phaseIndex = 0, ..}
                  | otherwise -> continue phase
                MapYes{..} -> continue $ case mapMaybe isButtonPress sdl of
                  (phaseJoystick, phaseButtonYes) : _ -> MapNo{..}
                  [] -> if any (== NC.EventCharacter 'q') cur
                    then Waiting{..}
                    else phase
                MapNo{..} -> continue $ case [ btnNo | (joy', btnNo) <- mapMaybe isButtonPress sdl, phaseJoystick == joy' ] of
                  buttonNo : _ -> TypeName
                    { phaseNewPlayer = Player
                      { playerName = ""
                      , playerJoystick = phaseJoystick
                      , playerYes = phaseButtonYes
                      , playerNo = buttonNo
                      }
                    , ..
                    }
                  [] -> if any (== NC.EventCharacter 'q') cur
                    then MapYes{..}
                    else phase
                TypeName{..} -> let
                  chars = [ c | NC.EventCharacter c <- cur ]
                  funs = flip map chars $ \c -> if c == '\DEL'
                    then \s -> take (length s - 1) s
                    else if c == '\n'
                      then id
                      else (++ [c])
                  name' = take 20 $ foldl (flip ($)) (playerName phaseNewPlayer) funs
                  updatedPlayer = phaseNewPlayer{ playerName = name' }
                  in if null chars
                    then continue phase
                    else if any (== '\n') chars
                      then continue $ Waiting{phasePlayers = phasePlayers ++ [updatedPlayer], ..}
                      else continue $ TypeName{phaseNewPlayer = updatedPlayer, ..}
                DeletePlayer{..}
                  | any (== NC.EventSpecialKey NC.KeyUpArrow) cur
                    -> continue $ DeletePlayer
                      { phaseIndex = max 0 $ phaseIndex - 1
                      , ..
                      }
                  | any (== NC.EventSpecialKey NC.KeyDownArrow) cur
                    -> continue $ DeletePlayer
                      { phaseIndex = min (length phasePlayers - 1) $ phaseIndex + 1
                      , ..
                      }
                  | any (== NC.EventCharacter 'q') cur
                    -> continue $ Waiting{..}
                  | any (== NC.EventCharacter '\n') cur
                    -> continue $ Waiting
                      { phasePlayers = case splitAt phaseIndex phasePlayers of
                        (xs, ys) -> xs ++ drop 1 ys
                      , ..
                      }
                  | otherwise -> continue phase
                AddTask{} -> undefined
                DeleteTask{} -> undefined
        let startState = Waiting{ phasePlayers = [], phaseTasks = [] }
        draw w startState
        loop startState
