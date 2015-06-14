{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module Main (main) where

import           Control.Applicative      ((<$>))
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.STM   (atomically, modifyTVar, newTVarIO,
                                           swapTVar)
import           Control.Exception        (bracket, bracket_)
import           Control.Monad            (forM, forever, liftM, unless)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import           Foreign                  (Ptr, alloca, nullPtr, peek, (.|.))
import           Foreign.C                (peekCString)
import qualified Graphics.UI.SDL          as SDL
import qualified Graphics.Vty             as Vty
import qualified Network.HTTP.Types       as HTTP
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.IO                (hIsTerminalDevice, stdout)

import           Core
import           Draw
import           Update

-- | Returns Just an event if there is one currently in the queue.
pollSDL :: (MonadIO m) => m (Maybe SDL.Event)
pollSDL = liftIO $ alloca $ \pevt -> SDL.pollEvent pevt >>= \case
  1 -> Just <$> peek pevt
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

newPresses :: [Set.Set Button360] -> [Set.Set Button360] -> [(SDL.JoystickID, Button360)]
newPresses prev curr = concat $ zipWith3 f [0..] prev curr where
  f i set1 set2 = map (i,) $ Set.toList $ Set.difference set2 set1

untilNothing :: (Monad m) => m (Maybe a) -> m [a]
untilNothing act = act >>= \case
  Just x  -> liftM (x :) $ untilNothing act
  Nothing -> return []

withVty :: Vty.Config -> (Vty.Vty -> IO a) -> IO a
withVty cfg = bracket (Vty.mkVty cfg) Vty.shutdown

main :: IO ()
main = do
  b <- hIsTerminalDevice stdout
  unless b $ error "Try again comrade. TTY is required"
  withSDL [SDL.SDL_INIT_JOYSTICK] $ do

    njoy <- SDL.numJoysticks
    joys <- forM [0 .. njoy - 1] $ notNull . SDL.joystickOpen

    cfg <- Vty.standardIOConfig
    withVty cfg $ \vty -> do
      vtyEvent <- newTVarIO []
      _ <- forkIO $ forever $ do
        e <- Vty.nextEvent vty
        atomically $ modifyTVar vtyEvent (e :)

      apiEvent <- newTVarIO []
      remote <- BL.readFile "remote/index.html"
      _ <- forkIO $ Warp.run 4200 $ \req f ->
        case map T.toUpper $ filter (not . T.null) $ Wai.pathInfo req of
          [code, "YES"] -> do
            atomically $ modifyTVar apiEvent ((T.unpack code, True) :)
            f $ Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")] "Received YES."
          [code, "NO"] -> do
            atomically $ modifyTVar apiEvent ((T.unpack code, False) :)
            f $ Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")] "Received NO."
          [] -> f $ Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/html" )] remote
          _  -> f $ Wai.responseLBS HTTP.status400 [(HTTP.hContentType, "text/plain")] "Invalid request."

      let loop :: Phase -> [Set.Set Button360] -> IO ()
          loop phase prev = do
            (w, h) <- Vty.displayBounds $ Vty.outputIface vty
            Vty.update vty $ draw w h prev phase
            liftIO $ threadDelay 5000
            sdlEvents <- untilNothing pollSDL
            vtyEvents <- atomically $ swapTVar vtyEvent []
            apiEvents <- atomically $ swapTVar apiEvent []
            let keys = [ k | Vty.EvKey k _ <- vtyEvents ]
                curr = foldr ($) prev $ map modifyButtons sdlEvents
            update (newPresses prev curr) keys apiEvents phase >>= \case
              Just phase' -> loop phase' curr
              Nothing     -> return ()

          startState = Waiting{ phasePlayers = [], phaseTasks = [] }
          startButtons = map (const Set.empty) joys

      loop startState startButtons
