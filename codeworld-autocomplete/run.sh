#!/bin/sh
cabal build
(cd ../codeworld-base && cabal haddock --hoogle)
SYMBOLS=../codeworld-base/dist/doc/html/codeworld-base/codeworld-base.txt
OUTPUT=../codeworld-server/web/autocomplete.txt
dist/build/codeworld-autocomplete/codeworld-autocomplete $SYMBOLS > $OUTPUT
