{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE RecordWildCards   #-}
module Draw (draw) where

import           Data.Fixed      (Milli)
import           Data.List       (intercalate)
import qualified Data.Set        as Set
import qualified Data.Time       as Time
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Vty    as Vty

import           Core

data Align
  = AlignL
  | AlignC
  | AlignR
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

align :: Align -> Int -> Vty.Attr -> Vty.Image -> Vty.Image
align alignment w style img = if
  | Vty.imageWidth img <= w -> let
    pad = w - Vty.imageWidth img
    padL = case alignment of
      AlignL -> 0
      AlignC -> pad `quot` 2
      AlignR -> pad
    padR = pad - padL
    in Vty.horizCat
      [ Vty.string style $ replicate padL ' '
      , img
      , Vty.string style $ replicate padR ' '
      ]
  | w < 4 -> case alignment of
    AlignR -> Vty.cropLeft  w img
    _      -> Vty.cropRight w img
  | otherwise -> Vty.horizCat $ case alignment of
    AlignR ->
      [ Vty.string style "..."
      , Vty.cropLeft (w - 3) img
      ]
    _ ->
      [ Vty.cropRight (w - 3) img
      , Vty.string style "..."
      ]

getPlayersTasks :: Phase -> [(Player, [Task])]
getPlayersTasks p = do
  (ix, player) <- zip [0..] $ phasePlayers p
  let tasks = [ task | (task, ixs) <- phaseTasks p, ix `elem` ixs ]
  return (player, tasks)

-- | Avoiding dumb type default warnings
rgb :: Int -> Int -> Int -> Vty.Color
rgb = Vty.rgbColor

on :: Vty.Color -> Vty.Color -> Vty.Attr
fg `on` bg = Vty.defAttr `Vty.withForeColor` fg `Vty.withBackColor` bg

mainScreen :: Int -> Int -> String -> Vty.Image
mainScreen w h message = Vty.vertCat
  [ Vty.charFill style ' ' w 1
  , align AlignC w style $ Vty.string style "GLORIOUS LABOR REGISTRY OF RUSTLER'S KEEP"
  , Vty.charFill style ' ' w $ h - 4
  , align AlignC w style $ Vty.string style message
  , Vty.charFill style ' ' w 1
  ] where style = rgb 255 255 255 `on` rgb 128 0 0

showButton :: [Set.Set Button360] -> Vty.Attr -> SDL.JoystickID -> Button360 -> Vty.Image
showButton btns style joy btn = Vty.string style' $ show btn
  where isPressed = case drop (fromIntegral joy) btns of
          []      -> False
          set : _ -> Set.member btn set
        style' = if isPressed then style `Vty.withForeColor` Vty.green else style

workerBox :: Int -> [Set.Set Button360] -> Vty.Attr -> Player -> [Task] -> Vty.Image
workerBox w btns style player tasks = genWorkerBox w style img1 img2 where
  img1 = case player of
    PlayerJoy{..} -> Vty.horizCat
      [ Vty.string nameStyle playerName
      , Vty.string style $ ": joystick " ++ show playerJoystick ++ ", "
      , showButton btns style playerJoystick playerYes
      , Vty.string style " for yes, "
      , showButton btns style playerJoystick playerNo
      , Vty.string style " for no"
      ]
    PlayerAPI{..} -> Vty.horizCat
      [ Vty.string nameStyle playerName
      , Vty.string style ": web code "
      , Vty.string (style `Vty.withForeColor` Vty.blue) playerCode
      ]
  img2 = Vty.string style $ case tasks of
    [] -> "Idle worker"
    _  -> intercalate ", " tasks
  nameStyle = style `Vty.withForeColor` Vty.red

taskBox :: Int -> Vty.Attr -> String -> Vty.Image
taskBox w style task = Vty.horizCat
  [ spaceColumn
  , align AlignL (w - 2) style $ Vty.string style task
  , spaceColumn
  ] where spaceColumn = Vty.charFill style ' ' 1 (1 :: Int)

newJoystickBox :: Int -> Vty.Attr -> Vty.Image
newJoystickBox w style = genWorkerBox w style img1 img2 where
  img1 = Vty.string nameStyle "New inspector"
  img2 = Vty.string style "Press YES button"
  nameStyle = style `Vty.withForeColor` Vty.red

newNoBox :: Int -> [Set.Set Button360] -> Vty.Attr -> SDL.JoystickID -> Button360 -> Vty.Image
newNoBox w btns style joy yes = genWorkerBox w style img1 img2 where
  img1 = Vty.horizCat
    [ Vty.string nameStyle "New inspector"
    , Vty.string style $ ": joystick " ++ show joy ++ ", "
    , showButton btns style joy yes
    , Vty.string style " for yes"
    ]
  img2 = Vty.string style "Press NO button"
  nameStyle = style `Vty.withForeColor` Vty.red

newJoyNameBox
  :: Int -> [Set.Set Button360] -> Vty.Attr
  -> SDL.JoystickID -> Button360 -> Button360 -> String -> Vty.Image
newJoyNameBox w btns style joy yes no name = genWorkerBox w style img1 img2 where
  img1 = Vty.horizCat
    [ Vty.string nameStyle name
    , Vty.string style $ ": joystick " ++ show joy ++ ", "
    , showButton btns style joy yes
    , Vty.string style " for yes, "
    , showButton btns style joy no
    , Vty.string style " for no"
    ]
  img2 = Vty.string style "Type inspector name"
  nameStyle = style `Vty.withForeColor` Vty.red

newAPINameBox :: Int -> Vty.Attr -> String -> Vty.Image
newAPINameBox w style name = genWorkerBox w style img1 img2 where
  img1 = Vty.horizCat
    [ Vty.string nameStyle name
    , Vty.string style $ ": web code "
    , Vty.string (style `Vty.withForeColor` Vty.blue) "????"
    ]
  img2 = Vty.string style "Type inspector name"
  nameStyle = style `Vty.withForeColor` Vty.red

genWorkerBox :: Int -> Vty.Attr -> Vty.Image -> Vty.Image -> Vty.Image
genWorkerBox w style line1 line2 = Vty.horizCat
  [ spaceColumn
  , Vty.vertCat $ map (align AlignL (w - 2) style) [line1, line2]
  , spaceColumn
  ] where spaceColumn = Vty.charFill style ' ' 1 (2 :: Int)

draw :: Int -> Int -> [Set.Set Button360] -> Phase -> Vty.Picture
draw _w _h btns phase = case phase of
  Waiting{..} -> standardScreen countMessage playerList
  ConfirmQuit{} -> standardScreen "Are you sure you want to quit? (y/n)" playerList
  AddPlayerYes{..} -> standardScreen countMessage $ playerList ++ [newJoystickBox $ _w - 2]
  AddPlayerNo{..} -> standardScreen countMessage $ playerList ++
    [\style -> newNoBox (_w - 2) btns style phaseJoystick phaseYes]
  AddPlayerName{..} -> standardScreen countMessage $ playerList ++
    [\style -> newJoyNameBox (_w - 2) btns style phaseJoystick phaseYes phaseNo phaseName]
  AddPlayerAPI{..} -> standardScreen countMessage $ playerList ++
    [\style -> newAPINameBox (_w - 2) style phaseName]
  DeletePlayer{} -> standardScreen "Choose a worker to remove from duty." playerList
  AddTask{..} -> standardScreen "Enter the new task to be performed." $ taskList ++
    [\style -> taskBox (_w - 2) style phaseNewTask]
  DeleteTask{..} -> standardScreen "Choose a task to remove." taskList
  Voting{..} -> flip standardScreen playerList $ let
    remaining = phaseVoteLength - Time.diffUTCTime phaseTimeNow phaseTimeStart
    in "Vote now! " ++ showTime remaining ++ " left"
  VoteComplete{..} -> standardScreen "Voting is over." playerList
  Inspection{..} -> flip standardScreen playerList $ let
    inspectionDone = length (phasePlayersGood ++ phasePlayersBad) == length phasePlayers
    in if inspectionDone then "Inspection complete."
      else "Inspection is underway. " ++ showTime (Time.diffUTCTime phaseTimeNow phaseTimeStart)
  ChosenOne{..} -> standardScreen "Your name was pulled!" playerList
  where
    countMessage = case length $ phasePlayers phase of
      1 -> "1 inspector ready."
      n -> show n ++ " inspectors ready."
    standardScreen msg entries = Vty.picForLayers
      [ Vty.horizCat
        [ Vty.backgroundFill 1 _h
        , Vty.vertCat
          [ Vty.backgroundFill (_w - 1) 3
          , Vty.vertCat $ zipWith ($) entries twoToneList
          ]
        ]
      , mainScreen _w _h msg
      ]
    twoToneList = do
      (ix, bg) <- zip [0..] $ cycle [rgb 255 255 255, rgb 200 200 200]
      return $ Vty.black `on` case phase of
        DeletePlayer{..} -> if phaseIndex == ix then Vty.cyan else bg
        ChosenOne{..}    -> if phaseIndex == ix then Vty.cyan else bg
        DeleteTask{..}   -> if phaseIndex == ix then Vty.cyan else bg
        Voting{..}       -> if
          | ix `elem` phasePlayersYes -> rgb 100 255 100
          | ix `elem` phasePlayersNo  -> rgb 255 100 100
          | otherwise                 -> bg
        VoteComplete{..} -> if
          | ix `elem` phasePlayersYes -> rgb 100 255 100
          | ix `elem` phasePlayersNo  -> rgb 255 100 100
          | otherwise                 -> bg
        Inspection{..}   -> if
          | ix `elem` map fst phasePlayersGood -> rgb 100 255 100
          | ix `elem` map fst phasePlayersBad  -> rgb 255 100 100
          | otherwise                          -> bg
        _                -> bg
    playerList :: [Vty.Attr -> Vty.Image]
    playerList = do
      (p, tasks) <- getPlayersTasks phase
      return $ \style -> workerBox (_w - 2) btns style p tasks
    taskList :: [Vty.Attr -> Vty.Image]
    taskList = do
      (task, _) <- phaseTasks phase
      return $ \style -> taskBox (_w - 2) style task
    showTime :: Time.NominalDiffTime -> String
    showTime t = show (realToFrac t :: Milli) ++ "s"
