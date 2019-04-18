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

source base.sh

for i in $(grep -l -i Test-Suite */*.cabal); do
    TARGET=$(dirname $i)

    run $TARGET cabal_install --enable-tests --only-dependencies
    run $TARGET cabal_configure --enable-tests
    run $TARGET cabal test

    if [ $TARGET == "codeworld-api" ]; then
        run $TARGET cabal_install --ghcjs --enable-tests --only-dependencies
        run $TARGET cabal_configure --ghcjs --enable-tests
        run $TARGET cabal test
    fi
done
