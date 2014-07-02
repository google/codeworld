#!/bin/sh
fuser -k -n tcp $1
rm -rf user/base.jsexe
./dist/build/codeworld-server/codeworld-server -p $1
