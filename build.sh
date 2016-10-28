#!/bin/bash

# Copyright 2016 The CodeWorld Authors. All rights reserved.
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

run .  cabal_install --ghcjs ./codeworld-api ./codeworld-base ./codeworld-game-api
run codeworld-base  cabal configure --ghcjs
run codeworld-base  cabal haddock --html
run codeworld-base  cabal haddock --hoogle
run codeworld-api   cabal configure --ghcjs
run codeworld-api   cabal haddock --html
run codeworld-api   cabal haddock --hoogle

# Build codeworld-server from this project.

run .  cabal_install ./codeworld-server
run .  cabal_install ./codeworld-game-api
run .  cabal_install ./codeworld-game-server

# Build the JavaScript client code for FunBlocks, the block-based UI.
run .  cabal_install --ghcjs ./funblocks-client
