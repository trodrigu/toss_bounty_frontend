make: src/Main.elm
		./env.sh > _build/env.js && elm-live --output=_build/elm.js src/Main.elm --pushstate --open --debug --dir=_build
