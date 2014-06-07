#!/bin/sh
fuser -k -n tcp $1
./dist/build/codeworld-server/codeworld-server -p $1
