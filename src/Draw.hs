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
  AddTask{..} -> Vty.picForImage $ Vty.vertCat
    [ taskList
    , blank
    , Vty.string Vty.defAttr "Enter new task name"
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.cyan `Vty.withForeColor` Vty.white) phaseNewTask
    , blank -- reset color
    ]
  DeleteTask{..} -> Vty.picForImage $ Vty.vertCat
    [ taskList
    , blank
    , Vty.string Vty.defAttr "Remove which task?"
    ]
  Voting{..} -> Vty.picForImage $ Vty.vertCat
    [ Vty.string Vty.defAttr $ let
      remaining = phaseVoteLength - Time.diffUTCTime phaseTimeNow phaseTimeStart
      in "Vote now! " ++ showTime remaining ++ " seconds left"
    , blank
    , showVote phasePlayersYes phasePlayersNo
    ]
  VoteComplete{..} -> Vty.picForImage $ Vty.vertCat
    [ Vty.string Vty.defAttr "Voting is over."
    , blank
    , showVote phasePlayersYes phasePlayersNo
    ]
  Inspection{..} -> Vty.picForImage $ Vty.vertCat
    [ Vty.string Vty.defAttr $ if inspectionDone then "Inspection complete." else "Inspection is underway."
    , Vty.string Vty.defAttr $ "Time: " ++ if inspectionDone
      then let
        allTimes = [ t | (_, t) <- phasePlayersGood ++ phasePlayersBad ]
        in showTime $ Time.diffUTCTime (foldr max phaseTimeStart allTimes) phaseTimeStart
      else showTime $ Time.diffUTCTime phaseTimeNow phaseTimeStart
    , blank
    , Vty.vertCat $ flip map (zip [0..] phasePlayers) $ \(i, player) -> let
        good = [ time | (j, time) <- phasePlayersGood, i == j ]
        bad  = [ time | (j, time) <- phasePlayersBad , i == j ]
        tasks = [ task | (task, ixs) <- phaseTasks, i `elem` ixs ]
        attr = case (good, bad) of
          ([]   , []   ) -> Vty.defAttr
          (_ : _, _    ) -> Vty.defAttr `Vty.withForeColor` Vty.green
          (_    , _ : _) -> Vty.defAttr `Vty.withForeColor` Vty.white `Vty.withBackColor` Vty.red
        in Vty.vertCat
          [ Vty.string attr $ case good ++ bad of
            time : _ -> simpleShowPlayer player ++ " (" ++ showTime (Time.diffUTCTime time phaseTimeStart) ++ ")"
            []       -> simpleShowPlayer player
          , Vty.vertCat [ Vty.string attr $ "  " ++ task | task <- tasks ]
          ]
    , blank -- reset color
    ] where inspectionDone = length (phasePlayersGood ++ phasePlayersBad) == length phasePlayers
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
      let selected = case phase of
            DeletePlayer{..} -> Just phaseIndex
            ChosenOne{..}    -> Just phaseIndex
            DeleteTask{..}   -> Just phaseIndex
            _                -> Nothing
      (ix, bg) <- zip [0..] $ cycle [rgb 255 255 255, rgb 200 200 200]
      return $ Vty.black `on` if selected == Just ix then Vty.cyan else bg
    playerList :: [Vty.Attr -> Vty.Image]
    playerList = do
      (p, tasks) <- getPlayersTasks phase
      return $ \style -> workerBox (_w - 2) btns style p tasks
    blank = Vty.string Vty.defAttr ""
    showVote playersYes playersNo = Vty.vertCat
      [ Vty.string (Vty.defAttr `Vty.withForeColor` Vty.green) $ "YEA (" ++ show (length playersYes) ++ "):"
      , Vty.vertCat $ flip map playersYes $ \ix ->
          Vty.string Vty.defAttr $ "  " ++ simpleShowPlayer (phasePlayers phase !! ix)
      , blank
      , Vty.string (Vty.defAttr `Vty.withForeColor` Vty.red) $ "NAY (" ++ show (length playersNo) ++ "):"
      , Vty.vertCat $ flip map playersNo $ \ix ->
          Vty.string Vty.defAttr $ "  " ++ simpleShowPlayer (phasePlayers phase !! ix)
      , blank
      , Vty.string (Vty.defAttr `Vty.withForeColor` Vty.yellow) $ "??? (" ++ show (length undecided) ++ "):"
      , Vty.vertCat $ flip map undecided $ \ix ->
          Vty.string Vty.defAttr $ "  " ++ simpleShowPlayer (phasePlayers phase !! ix)
      ] where undecided = [ i | i <- [0 .. length (phasePlayers phase) - 1], not $ elem i playersYes || elem i playersNo ]
    simpleShowPlayer = \case
      PlayerAPI{..} -> playerName ++ " (" ++ playerCode ++ ")"
      PlayerJoy{..} -> playerName ++ " (joy " ++ show playerJoystick ++ ", " ++ show playerYes ++ ", " ++ show playerNo ++ ")"
    showTime :: Time.NominalDiffTime -> String
    showTime t = show (realToFrac t :: Milli)
    taskList = Vty.vertCat
      [ Vty.string Vty.defAttr "Tasks:"
      , imageTasks
      ]
    imageTasks = Vty.vertCat $ zipWith taskLine [0..] $ map fst $ phaseTasks phase
    taskLine i task = Vty.horizCat
      [ Vty.string Vty.defAttr $ case phase of
          DeleteTask{..} | phaseIndex == i -> "* "
          _                                -> "  "
      , Vty.string Vty.defAttr task
      ]
