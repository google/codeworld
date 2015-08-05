#!/bin/sh

USER_BUILD=data/user

fuser -k -n tcp $1
rm -rf $USER_BUILD/*.jsexe
rm -rf $USER_BUILD/*.js_hi
rm -rf $USER_BUILD/*.js_o
rm -rf $USER_BUILD/*.err.txt
./dist/build/codeworld-server/codeworld-server -p $1
