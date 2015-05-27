{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Control.Exception (bracket_)
import Control.Monad
import Control.Monad.Fix
import UI.NCurses
import Control.Monad.IO.Class

-- | Returns Just an event if there is one currently in the queue.
pollEvent :: (MonadIO m) => m (Maybe SDL.Event)
pollEvent = liftIO $ alloca $ \pevt -> SDL.pollEvent pevt >>= \case
  1 -> fmap Just $ peek pevt
  _ -> return Nothing

pattern KeyPress scan <- SDL.KeyboardEvent
  { SDL.eventType           = SDL.SDL_KEYDOWN
  , SDL.keyboardEventRepeat = 0
  , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymScancode = scan }
  }

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

main :: IO ()
main = withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_JOYSTICK, SDL.SDL_INIT_HAPTIC, SDL.SDL_INIT_EVENTS] $ do
  runCurses $ do
    w <- defaultWindow
    njoy <- SDL.numJoysticks
    _joys <- forM [0 .. njoy - 1] $ notNull . SDL.joystickOpen
    nhap <- SDL.numHaptics
    _haps <- forM [0 .. nhap - 1] $ notNull . SDL.hapticOpen
    fix $ \loop -> do
      evt <- liftIO $ alloca $ \pevt -> do
        code 1 $ SDL.waitEvent pevt
        peek pevt
      case evt of
        SDL.JoyButtonEvent
          { SDL.joyButtonEventButton = button
          , SDL.joyButtonEventState = SDL.SDL_PRESSED
          } -> case button of
            0 -> message "A"
            1 -> message "B"
            2 -> message "X"
            3 -> message "Y"
            4 -> message "LB"
            5 -> message "RB"
            6 -> message "Left Click"
            7 -> message "Right Click"
            8 -> message "Start"
            9 -> message "Back"
            10 -> return () -- xbox button: exit
            11 -> message "D-pad Up"
            12 -> message "D-pad Down"
            13 -> message "D-pad Left"
            14 -> message "D-pad Right"
            _ -> message $ show button
            where message s = do
                    updateWindow w $ do
                      moveCursor 0 0
                      drawLineH (Just $ Glyph ' ' []) 1000
                      moveCursor 0 0
                      drawString s
                    render
                    loop
        _ -> loop
