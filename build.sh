#!/bin/bash

# Copyright 2018 The CodeWorld Authors. All rights reserved.
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

cwd=$(pwd)

source base.sh

run .  cabal update

# Install the codeworld-base and codeworld-api packages

run .  cabal_install --ghcjs ./codeworld-prediction \
                             ./codeworld-error-sanitizer \
                             ./codeworld-api \
                             ./codeworld-base \
                             ./codeworld-game-api \
                             QuickCheck

run codeworld-base  cabal configure --ghcjs
run codeworld-base  cabal haddock --html
run codeworld-base  cabal haddock --hoogle

# Work-around for haddock dropping pattern synonyms in hoogle output.
grep -r -s -h 'pattern\s*[A-Za-z_0-9]*\s*::.*' codeworld-base/ \
    >> web/codeworld-base.txt

run codeworld-api   cabal configure --ghcjs
run codeworld-api   cabal haddock --html
run codeworld-api   cabal haddock --hoogle

# Build codeworld-server from this project.

run .  cabal_install ./codeworld-server \
                     ./codeworld-error-sanitizer \
                     ./codeworld-compiler \
                     ./codeworld-game-api \
                     ./codeworld-prediction \
                     ./codeworld-api \
                     ./codeworld-game-server \
                     ./codeworld-account \
                     ./codeworld-auth

# Build the JavaScript client code for FunBlocks, the block-based UI.
run .  cabal_install --ghcjs ./funblocks-client

# Build the CodeMirror JavaScript bundle.
function build_codemirror {
  bin/compress codemirror \
               haskell \
               active-line \
               annotatescrollbar \
               dialog \
               match-highlighter \
               matchbrackets \
               matchesonscrollbar \
               placeholder \
               rulers \
               runmode \
               search \
               searchcursor \
               show-hint \
    --local node_modules/uglify-js/bin/uglifyjs \
    > codemirror-compressed.js
}

run $BUILD/CodeMirror build_codemirror
