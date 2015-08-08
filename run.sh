#!/bin/bash

source base.sh

# Install the codeworld-base package

run .  cabal_install --ghcjs ./codeworld-base
run codeworld-base  cabal haddock --html
run codeworld-base  cabal haddock --hoogle

# Build and run the autocomplete generator.

AC_SYMBOLS=codeworld-base/dist/doc/html/codeworld-base/codeworld-base.txt
AC_OUTPUT=codeworld-server/web/autocomplete.txt

run .  cabal_install ./codeworld-autocomplete
run .  codeworld-autocomplete $AC_SYMBOLS > $AC_OUTPUT

# Build codeworld-server from this project.

run .  cabal_install ./codeworld-server

# Run the server.

USER_BUILD=codeworld-server/data/user

fuser -k -n tcp 8080
rm -rf $USER_BUILD/*.jsexe
rm -rf $USER_BUILD/*.js_hi
rm -rf $USER_BUILD/*.js_o
rm -rf $USER_BUILD/*.err.txt

run codeworld-server  codeworld-server -p 8080
