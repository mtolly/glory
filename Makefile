.PHONY: mac

mac:
	brew install sdl2 sdl2_mixer sdl2_ttf
	stack build --flag glory:SDLDisplay
	cp .stack-work/install/x86_64-osx/*/*/bin/glory Glory.app/Contents/MacOS/glory
	strip Glory.app/Contents/MacOS/glory
	dylibbundler -cd -of -b -x Glory.app/Contents/MacOS/glory -d Glory.app/Contents/libs
