all: client server

client:
	elm-make src/Main.elm --output static/main.js

server:
	nix-build shell.nix

run: server
	result/bin/tiny-files-exe

deploy: client
	@read -p "Deploy to: " server; \
	scp static/main.js $$server:~/tiny-files/static/main.js
