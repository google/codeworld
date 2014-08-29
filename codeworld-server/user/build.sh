#!/bin/sh
ghcjs --no-native -Wall -O2 -fno-warn-deprecated-flags -fno-warn-amp           \
    -fno-warn-missing-signatures -fno-warn-incomplete-patterns                 \
    -fno-warn-unused-matches -hide-package base -package codeworld-base        \
    -XRebindableSyntax -XImplicitPrelude -XOverloadedStrings                   \
    -XNoTemplateHaskell -XNoUndecidableInstances -XNoQuasiQuotes               \
    -XForeignFunctionInterface -XJavaScriptFFI -XParallelListComp              \
    -XDisambiguateRecordFields -XNoMonomorphismRestriction                     \
    -XScopedTypeVariables -XBangPatterns -XPatternGuards -XViewPatterns        \
    -XRankNTypes -XExistentialQuantification -XKindSignatures -XEmptyDataDecls \
    -XLiberalTypeSynonyms -XTypeOperators -XRecordWildCards -XNamedFieldPuns   \
    --no-rts --no-stats --use-base=base.jsexe/out.base.symbs $1.hs             \
    2> $1.err.txt
