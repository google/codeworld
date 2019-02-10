#!/bin/bash

# Copyright 2019 The CodeWorld Authors. All rights reserved.
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
run $BUILD npm install --silent js-beautify

function format {
    sed -i 's/\bvar\b/let/' $(find web/js -regex .*\\.js$ -type f)
    sed -i 's/: function\b\s*\([(][^)]*[)]\)\s*[{]/\1 {/' $(find web/js -regex .*\\.js$ -type f)
    sed -i 's/\bfunction\b\s*\([(][^)]*[)]\)\s*[{]/\1 => {/' $(find web/js -regex .*\\.js$ -type f)
    sed -i 's/[(]\([a-zA-Z$_][a-zA-Z0-9$_]*\)[)] =>/\1 =>/' $(find web/js -regex .*\\.js$ -type f)

    nodejs build/node_modules/js-beautify/js/bin/js-beautify.js $(find web/js -regex .*\\.js$ -type f)
    nodejs build/node_modules/js-beautify/js/bin/css-beautify.js $(find web/css -regex .*\\.css$ -type f)
    nodejs build/node_modules/js-beautify/js/bin/html-beautify.js $(find web -regex .*\\.html$ -type f)
}

run . format
