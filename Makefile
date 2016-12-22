all: client server

client:
	elm-make src/Main.elm --output static/main.js

server:
	stack build

run:
	stack exec tiny-files-exe
