#!/bin/sh
ls *.hs | sed s/\\.hs// \
        | awk '{print "rm -rf "$1".jsexe ; ghcjs -hide-package base -package codeworld-base -XRebindableSyntax -XImplicitPrelude -XOverloadedStrings "$1".hs"}' \
        | /bin/sh
