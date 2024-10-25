all: run

build:
	elm make --output=app.js src/Main.elm

run: build
	xdg-open index.html