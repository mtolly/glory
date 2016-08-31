{-# LANGUAGE TemplateHaskell #-}
module Resources where

import qualified Data.ByteString as B
import           Data.FileEmbed  (embedFile)

data SFX
  = SFX_printer_line
  | SFX_time_up
  | SFX_stamp_down
  | SFX_border_callguards
  | SFX_monster_yes
  | SFX_monster_no
  | SFX_tis_100_boot
  | SFX_pinball_8
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

sfxWAV :: SFX -> B.ByteString
sfxWAV sfx = case sfx of
  SFX_printer_line      -> $(embedFile "sound/papers/printer-line.wav")
  SFX_time_up           -> $(embedFile "sound/papers/time-up.wav")
  SFX_stamp_down        -> $(embedFile "sound/papers/stamp-down.wav")
  SFX_border_callguards -> $(embedFile "sound/papers/border-callguards.wav")
  SFX_monster_yes       -> $(embedFile "sound/steam-clicker-yes.wav")
  SFX_monster_no        -> $(embedFile "sound/steam-clicker-no.wav")
  SFX_tis_100_boot      -> $(embedFile "sound/tis-100-boot.wav")
  SFX_pinball_8         -> $(embedFile "sound/pinball/SOUND8.WAV")

theFont :: B.ByteString
theFont = $(embedFile "04B03-U--misaki_gothic.ttf")

theRemote :: B.ByteString
theRemote = $(embedFile "remote/index.html")
