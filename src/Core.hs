module Core
( Phase(..)
, Player(..)
, Task
, Button360(..)
, Ortho(..)
) where

import qualified Data.Time       as Time
import qualified Graphics.UI.SDL as SDL

data Phase
  = Waiting
    { phasePlayers :: [Player]
    , phaseTasks   :: [(Task, [Int])]
    }
  | ConfirmQuit
    { phasePlayers :: [Player]
    , phaseTasks   :: [(Task, [Int])]
    }
  | AddPlayerYes
    { phasePlayers :: [Player]
    , phaseTasks   :: [(Task, [Int])]
    }
  | AddPlayerNo
    { phaseJoystick :: SDL.JoystickID
    , phaseYes      :: Button360
    , phasePlayers  :: [Player]
    , phaseTasks    :: [(Task, [Int])]
    }
  | AddPlayerName
    { phaseJoystick :: SDL.JoystickID
    , phaseYes      :: Button360
    , phaseNo       :: Button360
    , phaseName     :: String
    , phasePlayers  :: [Player]
    , phaseTasks    :: [(Task, [Int])]
    }
  | AddPlayerAPI
    { phaseName    :: String
    , phasePlayers :: [Player]
    , phaseTasks   :: [(Task, [Int])]
    }
  | DeletePlayer
    { phaseIndex   :: Int
    , phasePlayers :: [Player]
    , phaseTasks   :: [(Task, [Int])]
    }
  | AddTask
    { phaseNewTask :: Task
    , phasePlayers :: [Player]
    , phaseTasks   :: [(Task, [Int])]
    }
  | DeleteTask
    { phaseIndex   :: Int
    , phasePlayers :: [Player]
    , phaseTasks   :: [(Task, [Int])]
    }
  | Voting
    { phasePlayersYes :: [Int]
    , phasePlayersNo  :: [Int]
    , phaseTimeStart  :: Time.UTCTime
    , phaseTimeNow    :: Time.UTCTime
    , phaseVoteLength :: Time.NominalDiffTime
    , phasePlayers    :: [Player]
    , phaseTasks      :: [(Task, [Int])]
    }
  | VoteComplete
    { phasePlayersYes :: [Int]
    , phasePlayersNo  :: [Int]
    , phasePlayers    :: [Player]
    , phaseTasks      :: [(Task, [Int])]
    }
  | Inspection
    { phasePlayersGood :: [(Int, Time.UTCTime)]
    , phasePlayersBad  :: [(Int, Time.UTCTime)]
    , phaseTimeStart   :: Time.UTCTime
    , phaseTimeNow     :: Time.UTCTime
    , phasePlayers     :: [Player]
    , phaseTasks       :: [(Task, [Int])]
    }
  | ChosenOne
    { phaseIndex   :: Int
    , phasePlayers :: [Player]
    , phaseTasks   :: [(Task, [Int])]
    }
  | Citation
    { phaseIndex   :: Int
    , phasePlayers :: [Player]
    , phaseTasks   :: [(Task, [Int])]
    }
  deriving (Eq, Ord, Show)

data Player
  = PlayerJoy
    { playerName      :: String
    , playerCitations :: Int
    , playerJoystick  :: SDL.JoystickID
    , playerYes       :: Button360
    , playerNo        :: Button360
    }
  | PlayerAPI
    { playerName      :: String
    , playerCitations :: Int
    , playerCode      :: String
    }
  deriving (Eq, Ord, Show, Read)

type Task = String

data Button360
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
  | Dpad Ortho
  | LTrigger
  | RTrigger
  | LStick Ortho
  | RStick Ortho
  deriving (Eq, Ord, Show, Read)

data Ortho = U | D | L | R
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
