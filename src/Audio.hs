{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Audio where

import qualified Data.ByteString        as B
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.FileEmbed         (embedFile)
import           Foreign                (castPtr, withMany)
import qualified Graphics.UI.SDL        as SDL

import           SDLMixer
import           SDLNice

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
sfxWAV = \case
  SFX_printer_line      -> $(embedFile "sound/papers/printer-line.wav")
  SFX_time_up           -> $(embedFile "sound/papers/time-up.wav")
  SFX_stamp_down        -> $(embedFile "sound/papers/stamp-down.wav")
  SFX_border_callguards -> $(embedFile "sound/papers/border-callguards.wav")
  SFX_monster_yes       -> $(embedFile "sound/steam-clicker-yes.wav")
  SFX_monster_no        -> $(embedFile "sound/steam-clicker-no.wav")
  SFX_tis_100_boot      -> $(embedFile "sound/tis-100-boot.wav")
  SFX_pinball_8         -> $(embedFile "sound/pinball/SOUND8.WAV")

withChunk :: SFX -> (MixChunk -> IO a) -> IO a
withChunk sfx act = unsafeUseAsCStringLen (sfxWAV sfx) $ \(wav, len) -> do
  rw <- notNull $ SDL.rwFromConstMem (castPtr wav) (fromIntegral len)
  notNull (mixLoadWAVRW rw 1) >>= act

withChunks :: ((SFX -> MixChunk) -> IO a) -> IO a
withChunks act = let
  sfxs = [minBound .. maxBound]
  in withMany withChunk sfxs $ \chunks -> let
    table = zip sfxs chunks
    in act $ \sfx -> case lookup sfx table of
      Just chunk -> chunk
      Nothing    -> error $ "withChunks: panic! no chunk found for " ++ show sfx
