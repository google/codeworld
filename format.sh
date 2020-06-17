#!/bin/bash

# Copyright 2020 The CodeWorld Authors. All rights reserved.
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

set -euo pipefail

source base.sh

mkdir -p $BUILD/node_modules

function formatall_js {
    npx prettier --write $(find web/js -regex .*\\.js$ -type f)
    npx prettier --write $(find web/css -regex .*\\.css$ -type f)
    npx prettier --write $(find web -regex .*\\.html$ -type f)

    npx eslint --fix $(find web/js -regex .*\\.js$ -type f) || true
}

run . formatall_js

function formatall_hs {
    for f in $(find */src -regex .*\\.hs$ -type f | grep -v '^ghcjs/')
    do
        ormolu --mode inplace $f
    done
}

run . formatall_hs
