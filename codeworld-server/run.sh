#!/bin/sh
fuser -k -n tcp $1
(cd user && ./clean.sh)
./dist/build/codeworld-server/codeworld-server -p $1
