module Audio
( SFX(..)
, withChunk
, withChunks
) where

import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Foreign                (castPtr, withMany)
import qualified SDL.Raw                as SDL

import           Resources
import           SDLMixer
import           SDLNice

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
