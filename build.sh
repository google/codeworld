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

cwd=$(pwd)

source base.sh

# Locally mirror various third-party JavaScript, CSS, etc, so that we don't
# have to have user's browsers retrieve code from random remote locations.

run .           mirror/get_mirrored

# Install the codeworld-base and codeworld-api packages
run .  cabal_install --ghcjs ./codeworld-prediction \
                             ./codeworld-error-sanitizer \
                             ./codeworld-api \
                             ./codeworld-base \
                             ./codeworld-game-api \
                             ./codeworld-available-pkgs

# Hide packages that conflict with modules in more popular choices.
run . ghcjs-pkg hide base-compat         # Use base-compat-batteries
run . ghcjs-pkg hide ghcjs-dom-jsffi     # Use ghcjs-dom
run . ghcjs-pkg hide matrices            # Conflicts with matrix
run . ghcjs-pkg hide simple-affine-space # Conflicts with vector-space
run . ghcjs-pkg hide newtype             # Replaced by newtype-generics
run . ghcjs-pkg hide non-empty           # Conflicts with semialign

# Check for duplicate modules.  Fail the build if so, since that's a
# poor user experience.
run . nodejs build/bin/find-dup-modules.jsexe/all.js \
             ~/.ghcjs/x86_64-linux-8.6.0.1-8.6.5/ghcjs/package.conf.d/package.cache

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
                     ./codeworld-requirements \
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
  node_modules/uglify-js/bin/uglifyjs \
      lib/codemirror.js \
      addon/dialog/dialog.js \
      addon/display/placeholder.js \
      addon/display/rulers.js \
      addon/edit/matchbrackets.js \
      addon/hint/show-hint.js \
      addon/lint/lint.js \
      addon/runmode/runmode.js \
      addon/scroll/annotatescrollbar.js \
      addon/search/match-highlighter.js \
      addon/search/matchesonscrollbar.js \
      addon/search/search.js \
      addon/search/searchcursor.js \
      addon/selection/active-line.js \
      mode/haskell/haskell.js \
      node_modules/codemirror-extension/addon/hover/text-hover.js \
      -c -m \
    > codemirror-compressed.js
  exitcode=$?
  if [ $exitcode -ne 0 ]; then
    cat codemirror-compressed.js
    rm codemirror-compressed.js
  fi
  return $exitcode
}

run $BUILD/CodeMirror build_codemirror
