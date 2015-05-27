{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Control.Exception (bracket_)
import Control.Concurrent (threadDelay)
import Control.Monad
import qualified UI.NCurses as NC
import Control.Monad.IO.Class

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

{-
360 buttons:
0 -> "A"
1 -> "B"
2 -> "X"
3 -> "Y"
4 -> "LB"
5 -> "RB"
6 -> "Left Click"
7 -> "Right Click"
8 -> "Start"
9 -> "Back"
10 -> "Xbox"
11 -> "D-pad Up"
12 -> "D-pad Down"
13 -> "D-pad Left"
14 -> "D-pad Right"
-}

main :: IO ()
main = withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_JOYSTICK, SDL.SDL_INIT_HAPTIC, SDL.SDL_INIT_EVENTS] $ do
  NC.runCurses $ do
    w <- NC.defaultWindow
    _ <- NC.setCursorMode NC.CursorInvisible
    NC.setEcho False
    njoy <- SDL.numJoysticks
    _joys <- forM [0 .. njoy - 1] $ notNull . SDL.joystickOpen
    nhap <- SDL.numHaptics
    _haps <- forM [0 .. nhap - 1] $ notNull . SDL.hapticOpen
    NC.updateWindow w $ NC.drawBox (Just NC.glyphLineV) (Just NC.glyphLineH)
    NC.render
    let loop tick = do
          liftIO $ threadDelay 5000
          evts <- pollAllEvents w
          case evts of
            ([], []) -> loop $ tick + 1
            _ -> do
              NC.updateWindow w $ do
                NC.moveCursor 1 1
                NC.drawString $ "Tick " ++ show tick
                NC.moveCursor 2 1
                NC.drawString $ show evts
              NC.render
              loop $ tick + 1
    loop (0 :: Integer)
