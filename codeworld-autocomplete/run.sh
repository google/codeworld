#!/bin/sh
cabal build
ghc -hide-package base -package codeworld-base -e ":browse Prelude" > symbols.txt
dist/build/codeworld-autocomplete/codeworld-autocomplete symbols.txt > ../codeworld-server/web/autocomplete.txt
