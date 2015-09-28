#!/bin/bash

# Copyright 2015 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

source base.sh

USER_BUILD=data/user

fuser -k -n tcp 8080
rm -rf $USER_BUILD/base.jsexe
rm -rf $USER_BUILD/LinkMain.js_hi
rm -rf $USER_BUILD/LinkMain.js_o
rm -rf $USER_BUILD/P??/*.jsexe
rm -rf $USER_BUILD/P??/*.js_hi
rm -rf $USER_BUILD/P??/*.js_o
rm -rf $USER_BUILD/P??/*.err.txt

mkdir -p log
run .  codeworld-server -p 8080
