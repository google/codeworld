#!/bin/sh
ghcjs -O2 -XRebindableSyntax -XOverloadedStrings -XImplicitPrelude -hide-package base -package codeworld-base $1.hs
# closure-compiler $1.jsexe/all.js > $1.jsexe/opt.js
cp index.html $1.jsexe/
