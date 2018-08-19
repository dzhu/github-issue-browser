build:
	elm-make src/Main.elm --output=elm.js --warn --yes

get-deps:
	elm-package install
