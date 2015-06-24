#!/bin/bash
set -e
set -u

brew install sdl2 sdl2_mixer

cabal sandbox init
cabal install --only-dep . ./glory-resources --flags SDLDisplay
cabal install ./glory-resources
cabal configure --flags SDLDisplay
cabal build
cp dist/build/glory/glory Glory.app/Contents/MacOS/glory
strip Glory.app/Contents/MacOS/glory
dylibbundler -cd -of -b -x Glory.app/Contents/MacOS/glory -d Glory.app/Contents/libs
