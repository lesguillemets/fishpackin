./build/Main.js: ./src/Main.hs
	hastec -Wall -fno-warn-unused-do-bind -O2 ./src/Main.hs -isrc -o ./build/Main.js
