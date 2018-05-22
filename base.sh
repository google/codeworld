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

BUILD=$(pwd)/build
DOWNLOADS=$BUILD/downloads

export PATH=$BUILD/bin:$PATH
export LANG=${LANG:-C.UTF-8}
export PREFIX=$BUILD

function run {
  local old_pwd=$PWD
  local temp_pwd
  local quiet=0

  if [ "$1" = "--quiet" ]; then
    quiet=1
    shift
  fi

  temp_pwd=$1
  shift

  if [ $quiet -eq 0 ]; then
    echo RUNNING: $@
  fi

  cd $temp_pwd
  $@
  if [ $? -ne 0 ]; then
    echo ============================
    echo = Aborting: Command failed =
    echo ============================

    exit 1
  fi
  cd $old_pwd
}

function cabal_install {
  cabal install --disable-library-profiling --force-reinstalls --global --prefix=$BUILD $@
}
