module SDLNice where

import           Control.Exception      (bracket_)
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign                (Ptr, nullPtr, (.|.))
import           Foreign.C              (peekCString)
import qualified Graphics.UI.SDL        as SDL

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
