module SDLMixer where

import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL

#include <SDL_mixer.h>

mixDefaultFormat :: Word16
mixDefaultFormat = {#const MIX_DEFAULT_FORMAT#}

{#fun Mix_Init as ^ { `CInt' } -> `CInt' #}
{#fun Mix_Quit as ^ {} -> `()' #}
{#fun Mix_OpenAudio as ^ { `CInt', `Word16', `CInt', `CInt' } -> `CInt' #}
{#fun Mix_CloseAudio as ^ {} -> `()' #}

{#pointer *Mix_Chunk as MixChunk #}
{#fun Mix_LoadWAV_RW as ^ { castPtr `Ptr SDL.RWops', `CInt' } -> `MixChunk' #}
mixLoadWAV :: CString -> IO MixChunk
mixLoadWAV s = withCString "rb" $ \rb -> do
  rw <- SDL.rwFromFile s rb
  mixLoadWAVRW rw 1
{#fun Mix_QuickLoad_WAV as ^ { castPtr `Ptr Word8' } -> `MixChunk' #}
{#fun Mix_FreeChunk as ^ { `MixChunk' } -> `()' #}

{#fun Mix_PlayChannelTimed as ^ { `CInt', `MixChunk', `CInt', `CInt' } -> `CInt' #}
mixPlayChannel :: CInt -> MixChunk -> CInt -> IO CInt
mixPlayChannel a b c = mixPlayChannelTimed a b c (-1)
