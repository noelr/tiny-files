client:
	elm-make src/Main.elm --output static/main.js

server:
	stack build
