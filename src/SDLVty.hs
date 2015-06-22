{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module SDLVty where

import qualified Graphics.UI.SDL as SDL
import SDLNice
import Data.List (transpose)
import Foreign
import Foreign.C
import Control.Monad (forM_)
import Control.Concurrent.STM
import Data.Char (toLower)
import qualified Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.TTF.FFI (TTFFont)

data Image = Image
  { imageWidth :: Int
  , imageHeight :: Int
  , imageData :: [[(Char, Attr)]] -- [row]
  } deriving (Eq, Show)
type Attr = (Color, Maybe Color)
type Color = SDL.Color
type Config = ()
type Input = ()
type Output = SDL.Window
type DisplayRegion = (Int, Int)

horizCat :: [Image] -> Image
horizCat imgs = Image
  { imageWidth = sum $ map imageWidth imgs
  , imageHeight = foldr max 0 $ map imageHeight imgs
  , imageData = map concat $ transpose $ map imageData imgs
  }

vertCat :: [Image] -> Image
vertCat imgs = Image
  { imageWidth = foldr max 0 $ map imageWidth imgs
  , imageHeight = sum $ map imageHeight imgs
  , imageData = concat $ map imageData imgs
  }

string :: Attr -> String -> Image
string attr str = Image
  { imageWidth = length str
  , imageHeight = 1
  , imageData = [zip str $ repeat attr]
  }

cropLeft :: Int -> Image -> Image
cropLeft w img = Image
  { imageWidth = min w $ imageWidth img
  , imageHeight = imageHeight img
  , imageData = map (reverse . take w . reverse) $ imageData img
  }

cropRight :: Int -> Image -> Image
cropRight w img = Image
  { imageWidth = min w $ imageWidth img
  , imageHeight = imageHeight img
  , imageData = map (take w) $ imageData img
  }

rgbColor :: (Integral a) => a -> a -> a -> Color
rgbColor r g b = SDL.Color (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

defAttr :: Attr
defAttr = (white, Nothing)

withForeColor :: Attr -> Color -> Attr
(_, bg) `withForeColor` fg = (fg, bg)

withBackColor :: Attr -> Color -> Attr
(fg, _) `withBackColor` bg = (fg, Just bg)

charFill :: Attr -> Char -> Int -> Int -> Image
charFill attr c w h = Image
  { imageWidth = w
  , imageHeight = h
  , imageData = replicate h $ replicate w (c, attr)
  }

backgroundFill :: Int -> Int -> Image
backgroundFill = charFill defAttr ' '

green, blue, red, black, cyan, white :: Color
green = SDL.Color 0 255 0 255
blue = SDL.Color 0 0 255 255
red = SDL.Color 255 0 0 255
black = SDL.Color 0 0 0 255
cyan = SDL.Color 0 255 255 255
white = SDL.Color 255 255 255 255

cellWidth, cellHeight :: Int
cellWidth = 12
cellHeight = 20

drawImage :: Image -> TTFFont -> SDL.Renderer -> IO ()
drawImage img font rend = do
  forM_ (zip [0..] $ imageData img) $ \(r, row) ->
    forM_ (zip [0..] row) $ \(c, (char, (fg, bg))) -> do
      let rect = SDL.Rect
            (fromIntegral $ c * cellWidth)
            (fromIntegral $ r * cellHeight)
            (fromIntegral cellWidth)
            (fromIntegral cellHeight)
      -- draw bg
      case bg of
        Nothing -> return ()
        Just (SDL.Color vr vg vb va) -> do
          zero $ SDL.setRenderDrawColor rend vr vg vb va
          zero $ with rect $ SDL.renderFillRect rend
      -- draw char
      case char of
        ' ' -> return ()
        _ -> do
          psurf <- notNull $ TTF.renderTextSolid font [char] fg
          ptex <- notNull $ SDL.createTextureFromSurface rend psurf
          SDL.freeSurface psurf
          zero $ with rect $ SDL.renderCopy rend ptex nullPtr
          SDL.destroyTexture ptex

mkVty :: Config -> IO Vty
mkVty () = do
  zero $ SDL.initSubSystem SDL.SDL_INIT_VIDEO
  zero TTF.init
  font <- notNull $ TTF.openFont "saxmono.ttf" 15
  window <- notNull $ withCString "vty" $ \s -> notNull $ SDL.createWindow
    s -- title
    SDL.SDL_WINDOWPOS_UNDEFINED -- x
    SDL.SDL_WINDOWPOS_UNDEFINED -- y
    640 -- width
    480 -- height
    SDL.SDL_WINDOW_RESIZABLE -- flags
  rend <- notNull $ SDL.createRenderer window (-1) 0
  events <- newTQueueIO
  watch <- SDL.mkEventFilter $ \_ pevt -> do
    evt <- peek pevt
    case evt of
      SDL.KeyboardEvent{ SDL.keyboardEventState = SDL.SDL_PRESSED, .. } -> do
        let gotKey k = atomically . writeTQueue events $ EvKey k []
        case SDL.keysymKeycode keyboardEventKeysym of
          SDL.SDLK_RETURN -> gotKey KEnter
          SDL.SDLK_ESCAPE -> gotKey KEsc
          SDL.SDLK_SPACE -> gotKey $ KChar ' '
          SDL.SDLK_DELETE -> gotKey KDel
          SDL.SDLK_UP -> gotKey KUp
          SDL.SDLK_DOWN -> gotKey KDown
          SDL.SDLK_LEFT -> gotKey KLeft
          SDL.SDLK_RIGHT -> gotKey KRight
          SDL.SDLK_BACKSPACE -> gotKey KBS
          key -> SDL.getKeyName key >>= peekCString >>= \case
            [ch] -> gotKey $ KChar $ toLower ch
            _ -> return ()
      _ -> return ()
    return 0
  SDL.addEventWatch watch nullPtr
  return Vty
    { update = \pic -> do
      zero $ SDL.setRenderDrawColor rend 0 0 0 255
      zero $ SDL.renderClear rend
      forM_ (reverse $ picLayers pic) $ \img -> drawImage img font rend
      SDL.renderPresent rend
    , nextEvent = atomically $ readTQueue events
    , inputIface = ()
    , outputIface = window
    , refresh = return ()
    , shutdown = do
      SDL.destroyRenderer rend
      SDL.destroyWindow window
      TTF.closeFont font
      TTF.quit
      SDL.quitSubSystem SDL.SDL_INIT_VIDEO
    }

standardIOConfig :: IO Config
standardIOConfig = return ()

displayBounds :: Output -> IO DisplayRegion
displayBounds window = alloca $ \pw ->
  alloca $ \ph -> do
    SDL.getWindowSize window pw ph
    w <- peek pw
    h <- peek ph
    return (fromIntegral w `quot` cellWidth, fromIntegral h `quot` cellHeight)

-- *************************************
-- following is unmodified code from vty
-- *************************************

-- | Representations of non-modifier keys.
--
-- * KFun is indexed from 0 to 63. Range of supported FKeys varies by terminal and keyboard.
--
-- * KUpLeft, KUpRight, KDownLeft, KDownRight, KCenter support varies by terminal and keyboard.
--
-- * Actually, support for most of these but KEsc, KChar, KBS, and KEnter vary by terminal and
-- keyboard.
data Key = KEsc  | KChar Char | KBS | KEnter
         | KLeft | KRight | KUp | KDown
         | KUpLeft | KUpRight | KDownLeft | KDownRight | KCenter
         | KFun Int | KBackTab | KPrtScr | KPause | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KBegin | KMenu
    deriving (Eq,Show,Read,Ord)

-- | Modifier keys.  Key codes are interpreted such that users are more likely to
-- have Meta than Alt; for instance on the PC Linux console, 'MMeta' will
-- generally correspond to the physical Alt key.
data Modifier = MShift | MCtrl | MMeta | MAlt
    deriving (Eq,Show,Read,Ord)

-- | Mouse buttons.
--
-- \todo not supported.
data Button = BLeft | BMiddle | BRight
    deriving (Eq,Show,Ord)

-- | Events.
data Event
    = EvKey Key [Modifier]
    -- | \todo mouse events are not supported
    | EvMouse Int Int Button [Modifier]
    -- | if read from 'eventChannel' this is the size at the time of the signal. If read from
    -- 'nextEvent' this is the size at the time the event was processed by Vty. Typically these are
    -- the same, but if somebody is resizing the terminal quickly they can be different.
    | EvResize Int Int
    deriving (Eq,Show,Ord)

-- | The main object.  At most one should be created.
--
-- The use of Vty typically follows this process:
--
--    0. initialize vty
--
--    1. use the update equation of Vty to display a picture
--
--    2. repeat
--
--    3. shutdown vty.
--
-- An alternative to tracking the Vty instance is to use 'withVty' in "Graphics.Vty.Inline.Unsafe".
--
-- This does not assure any thread safety. In theory, as long as an update action is not executed
-- when another update action is already then it's safe to call this on multiple threads.
--
-- \todo Remove explicit `shutdown` requirement.
data Vty = Vty 
    { -- | Outputs the given Picture. Equivalent to 'outputPicture' applied to a display context
      -- implicitly managed by Vty. The managed display context is reset on resize.
      update :: Picture -> IO ()
      -- | Get one Event object, blocking if necessary. This will refresh the terminal if the event
      -- is a 'EvResize'.
    , nextEvent :: IO Event
      -- | The input interface. See 'Input'
    , inputIface :: Input
      -- | The output interface. See 'Output'
    , outputIface :: Output
      -- | Refresh the display. 'nextEvent' will refresh the display if a resize occurs.
      -- If other programs output to the terminal and mess up the display then the application might
      -- want to force a refresh.
    , refresh :: IO ()
      -- | Clean up after vty.
      -- The above methods will throw an exception if executed after this is executed.
    , shutdown :: IO () 
    }

-- | The type of images to be displayed using 'update'.  
--
-- Can be constructed directly or using `picForImage`. Which provides an initial instance with
-- reasonable defaults for picCursor and picBackground.
data Picture = Picture
    { picCursor :: Cursor
    , picLayers :: [Image]
    , picBackground :: Background
    }

instance Show Picture where
    show (Picture _ layers _ ) = "Picture ?? " ++ show layers ++ " ??"

-- | A 'Picture' has a background pattern. The background is either ClearBackground. Which shows the
-- layer below or is blank if the bottom layer. Or the background pattern is a character and a
-- display attribute. If the display attribute used previously should be used for a background fill
-- then use `currentAttr` for the background attribute.
--
-- \todo The current attribute is always set to the default attributes at the start of updating the
-- screen to a picture.
data Background
    = Background 
    { backgroundChar :: Char
    , backgroundAttr :: Attr
    }
     -- | A ClearBackground is: 
     --
     -- * the space character if there are remaining non-skip ops
     --
     -- * End of line if there are no remaining non-skip ops.
    | ClearBackground

-- | A picture can be configured either to not show the cursor or show the cursor at the specified
-- character position. 
--
-- There is not a 1 to 1 map from character positions to a row and column on the screen due to
-- characters that take more than 1 column.
--
-- todo: The Cursor can be given a (character,row) offset outside of the visible bounds of the
-- output region. In this case the cursor will not be shown.
data Cursor = 
      NoCursor
    | Cursor Int Int

-- | Create a picture for display for the given image. The picture will not have a displayed cursor
-- and no background pattern (ClearBackground) will be used.
picForImage :: Image -> Picture
picForImage i = Picture 
    { picCursor = NoCursor
    , picLayers = [i]
    , picBackground = ClearBackground
    }

-- | Create a picture for display with the given layers. Ordered top to bottom.
--
-- The picture will not have a displayed cursor and no background apttern (ClearBackgroun) will be
-- used.
-- 
-- The first 'Image' is the top layer.
picForLayers :: [Image] -> Picture
picForLayers is = Picture 
    { picCursor = NoCursor
    , picLayers = is
    , picBackground = ClearBackground
    }
