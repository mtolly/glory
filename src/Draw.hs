{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE RecordWildCards   #-}
module Draw (draw) where

import           Data.Char       (toLower)
import           Data.Fixed      (Milli)
import           Data.List       (intercalate)
import           Data.Maybe      (fromMaybe)
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

-- | not srs, just sufficient
cyrillicize :: String -> String
cyrillicize = let
  str = "АБЦДЕФГХИЖКЛМНОПQРСТУВЪЬЙЗ"
  mapping = zip ['A'..'Z'] str ++ zip ['a'..'z'] (map toLower str)
  in map $ \c -> fromMaybe c $ lookup c mapping

getPlayersTasks :: Phase -> [(Bool, Player, [Task])]
getPlayersTasks p = do
  let selected = case p of
        DeletePlayer{..} -> Just phaseIndex
        _                -> Nothing
  (ix, player) <- zip [0..] $ phasePlayers p
  let tasks = [ task | (task, ixs) <- phaseTasks p, ix `elem` ixs ]
  return (selected == Just ix, player, tasks)

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

registeredWorker :: Int -> [Set.Set Button360] -> Vty.Attr -> Player -> [Task] -> Vty.Image
registeredWorker w btns style player tasks = Vty.horizCat
  [ spaceColumn
  , Vty.vertCat
    [ align AlignL (w - 2) style $ let
      nameStyle = style `Vty.withForeColor` Vty.red
      in case player of
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
    , align AlignL (w - 2) style $ Vty.string style $ case tasks of
      [] -> "Idle worker"
      _  -> intercalate ", " tasks
    ]
  , spaceColumn
  ] where spaceColumn = Vty.charFill style ' ' 1 (2 :: Int)

draw :: Int -> Int -> [Set.Set Button360] -> Phase -> Vty.Picture
draw _w _h btns phase = case phase of
  Waiting{..} -> Vty.Picture
    { Vty.picCursor = Vty.NoCursor
    , Vty.picBackground = Vty.ClearBackground
    , Vty.picLayers =
      [ Vty.vertCat $ Vty.backgroundFill _w 3 : do
        (bg, (sel, p, tasks)) <- zip (cycle [rgb 255 255 255, rgb 200 200 200]) $ getPlayersTasks phase
        let style = Vty.defAttr
              `Vty.withForeColor` Vty.black
              `Vty.withBackColor` if sel then Vty.cyan else bg
            img = registeredWorker (_w - 2) btns style p tasks
        return $ Vty.horizCat [Vty.backgroundFill (1 :: Int) 1, img]
      , mainScreen _w _h $ case length phasePlayers of
          1 -> "1 inspector ready."
          n -> show n ++ " inspectors ready."
      ]
    }
  ConfirmQuit{..} -> Vty.picForImage $ Vty.string Vty.defAttr "Are you sure you want to quit? (y/n)"
  AddPlayerYes{..} -> Vty.picForImage $ Vty.vertCat
    [ playersAndTasks
    , blank
    , Vty.string Vty.defAttr "Adding new controller inspector. Press YES button"
    ]
  AddPlayerNo{..} -> Vty.picForImage $ Vty.vertCat
    [ playersAndTasks
    , blank
    , Vty.horizCat
      [ Vty.string Vty.defAttr $ "Joystick " ++ show phaseJoystick ++ ", "
      , Vty.string (color phaseJoystick phaseYes) $ show phaseYes
      , Vty.string Vty.defAttr " for YES. Press NO button"
      ]
    ]
  AddPlayerName{..} -> Vty.picForImage $ Vty.vertCat
    [ playersAndTasks
    , blank
    , Vty.horizCat
      [ Vty.string Vty.defAttr $ "Joystick " ++ show phaseJoystick ++ ", "
      , Vty.string (color phaseJoystick phaseYes) $ show phaseYes
      , Vty.string Vty.defAttr " for YES, "
      , Vty.string (color phaseJoystick phaseNo) $ show phaseNo
      , Vty.string Vty.defAttr " for NO. Enter name"
      ]
    , blank
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.cyan `Vty.withForeColor` Vty.white) phaseName
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red `Vty.withForeColor` Vty.white) $ cyrillicize phaseName
    , blank -- reset color
    ]
  AddPlayerAPI{..} -> Vty.picForImage $ Vty.vertCat
    [ playersAndTasks
    , blank
    , Vty.string Vty.defAttr "Adding new API inspector. Enter name"
    , blank
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.cyan `Vty.withForeColor` Vty.white) phaseName
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red `Vty.withForeColor` Vty.white) $ cyrillicize phaseName
    , blank -- reset color
    ]
  DeletePlayer{..} -> Vty.Picture
    { Vty.picCursor = Vty.NoCursor
    , Vty.picBackground = Vty.ClearBackground
    , Vty.picLayers =
      [ Vty.vertCat $ Vty.backgroundFill _w 3 : do
        (bg, (sel, p, tasks)) <- zip (cycle [rgb 255 255 255, rgb 200 200 200]) $ getPlayersTasks phase
        let style = Vty.defAttr
              `Vty.withForeColor` Vty.black
              `Vty.withBackColor` if sel then Vty.cyan else bg
            img = registeredWorker (_w - 2) btns style p tasks
        return $ Vty.horizCat [Vty.backgroundFill (1 :: Int) 1, img]
      , mainScreen _w _h "Choose a worker to remove from duty."
      ]
    }
  AddTask{..} -> Vty.picForImage $ Vty.vertCat
    [ playersAndTasks
    , blank
    , Vty.string Vty.defAttr "Enter new task name"
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.cyan `Vty.withForeColor` Vty.white) phaseNewTask
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red `Vty.withForeColor` Vty.white) $ cyrillicize phaseNewTask
    , blank -- reset color
    ]
  DeleteTask{..} -> Vty.picForImage $ Vty.vertCat
    [ playersAndTasks
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
  ChosenOne{..} -> Vty.picForImage $ Vty.vertCat
    [ Vty.string Vty.defAttr "A player has been chosen!"
    , blank
    , Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red `Vty.withForeColor` Vty.white) $
      "  " ++ playerName (phasePlayers !! phaseIndex)
    , blank -- reset color
    ]
  where
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
    playersAndTasks = Vty.vertCat
      [ Vty.string Vty.defAttr "Inspectors:"
      , imagePlayersTasks
      , blank
      , Vty.string Vty.defAttr "Tasks:"
      , imageTasks
      ]
    imagePlayersTasks = Vty.vertCat $ zipWith imagePlayerTasks [0..] $ getPlayersTasks phase
    imagePlayerTasks i (b, player, tasks) = Vty.horizCat
      [ Vty.string Vty.defAttr $ if b then "* " else "  "
      , Vty.vertCat
        [ imagePlayer i player
        , Vty.pad 2 0 0 0 $ Vty.vertCat $ map (Vty.string Vty.defAttr) tasks
        ]
      ]
    imagePlayer :: Int -> Player -> Vty.Image
    imagePlayer i PlayerJoy{..} = Vty.horizCat
      [ Vty.string (Vty.defAttr `Vty.withForeColor` nameColor i) playerName
      , Vty.string Vty.defAttr $ " (joystick " ++ show playerJoystick ++ ", "
      , Vty.string (color playerJoystick playerYes) $ show playerYes
      , Vty.string Vty.defAttr " for yes, "
      , Vty.string (color playerJoystick playerNo) $ show playerNo
      , Vty.string Vty.defAttr " for no)"
      ]
    imagePlayer i PlayerAPI{..} = Vty.horizCat
      [ Vty.string (Vty.defAttr `Vty.withForeColor` nameColor i) playerName
      , Vty.string Vty.defAttr $ " (code " ++ playerCode ++ ")"
      ]
    nameColor :: Int -> Vty.Color
    nameColor i = case i of
      0 -> rgb 234 60 60
      1 -> rgb 239 160 40
      2 -> rgb 226 226 59
      3 -> rgb 88 219 65
      4 -> rgb 48 232 232
      5 -> rgb 49 111 234
      6 -> rgb 148 78 229
      7 -> rgb 239 95 239
      _ -> rgb 170 132 99
    color joy btn = if Set.member btn $ btns !! fromIntegral joy
      then Vty.defAttr `Vty.withForeColor` case btn of
        A -> Vty.green
        B -> Vty.red
        X -> Vty.blue
        Y -> Vty.yellow
        _ -> Vty.magenta
      else Vty.defAttr
    imageTasks = Vty.vertCat $ zipWith taskLine [0..] $ map fst $ phaseTasks phase
    taskLine i task = Vty.horizCat
      [ Vty.string Vty.defAttr $ case phase of
          DeleteTask{..} | phaseIndex == i -> "* "
          _                                -> "  "
      , Vty.string Vty.defAttr task
      ]
