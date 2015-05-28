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
import Data.Maybe (mapMaybe)
import System.IO (hIsTerminalDevice, stdout)

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
  = Waiting [Player]
  | MapYes [Player]
  | MapNo SDL.JoystickID Button360 [Player]
  | TypeName Player [Player]
  deriving (Eq, Ord, Show, Read)

data Player = Player String SDL.JoystickID Button360 Button360
  deriving (Eq, Ord, Show, Read)

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
              let continue phase' = if phase == phase'
                    then loop phase
                    else do
                      (_rows, cols) <- NC.screenSize
                      NC.updateWindow w $ do
                        NC.moveCursor 2 0
                        NC.drawLineH (Just $ NC.Glyph ' ' []) cols
                        NC.moveCursor 1 0
                        NC.drawLineH (Just $ NC.Glyph ' ' []) cols
                        NC.moveCursor 0 0
                        NC.drawLineH (Just $ NC.Glyph ' ' []) cols
                        NC.drawString $ show phase'
                      NC.render
                      loop phase'
              case phase of
                Waiting players -> if any (== NC.EventCharacter 'p') cur
                  then continue $ MapYes players
                  else if any (== NC.EventCharacter 'q') cur
                    then return () -- press q to quit
                    else continue phase
                MapYes players -> case mapMaybe isButtonPress sdl of
                  (joy, btn) : _ -> continue $ MapNo joy btn players
                  [] -> continue phase
                MapNo joy btnYes players -> case [ btnNo | (joy', btnNo) <- mapMaybe isButtonPress sdl, joy == joy' ] of
                  btnNo : _ -> continue $ TypeName (Player "" joy btnYes btnNo) players
                  [] -> continue phase
                TypeName (Player name joy btnYes btnNo) players -> let
                  chars = [ c | NC.EventCharacter c <- cur ]
                  funs = flip map chars $ \c -> if c == '\DEL'
                    then \s -> take (length s - 1) s
                    else if c == '\n'
                      then id
                      else (++ [c])
                  name' = take 20 $ foldl (flip ($)) name funs
                  newPlayer = Player name' joy btnYes btnNo
                  in if null chars
                    then continue phase
                    else if any (== '\n') chars
                      then continue $ Waiting $ players ++ [newPlayer]
                      else continue $ TypeName newPlayer players
                    {-
              if any (== NC.EventCharacter 'q') cur
                then return () -- press q to quit
                else case (sdl, cur) of
                  ([], []) -> loop $ tick + 1
                  _ -> do
                    NC.updateWindow w $ do
                      NC.moveCursor 1 1
                      NC.drawString $ "Tick " ++ show tick
                      NC.moveCursor 2 1
                      NC.drawString $ show sdl
                      NC.moveCursor 3 1
                      NC.drawString $ show cur
                    NC.render
                    loop $ tick + 1
                    -}
        loop $ Waiting []
