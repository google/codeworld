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

BUILD=$(pwd)/build
DOWNLOADS=$BUILD/downloads

export PATH=$BUILD/bin:$PATH
export LANG=${LANG:-C.UTF-8}
export PREFIX=$BUILD

function run {
  OLD_PWD=$PWD
  cd $1
  shift
  echo RUNNING: $@
  $@
  if [ $? -ne 0 ]; then
    echo ============================
    echo = Aborting: Command failed =
    echo ============================

    exit 1
  fi
  cd $OLD_PWD
}

function cabal_install {
  cabal install --global --prefix=$BUILD --reorder-goals --max-backjumps=-1 $@
}
