all: client server

client:
	elm-make src/Main.elm --output static/main.js

server:
	stack build

run:
	stack exec tiny-files-exe

deploy: client
	@read -p "Deploy to: " server; \
	scp static/main.js $$server:~/tiny-files/static/main.js
