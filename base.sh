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

BUILD=$(pwd)/build
DOWNLOADS=$BUILD/downloads

export PATH=$BUILD/bin:$PATH
export LANG=${LANG:-C.UTF-8}
export PREFIX=$BUILD

setnormal=""
setred=""
setgreen=""
setyellow=""
if test -t 1; then
  ncolors=$(tput colors)
  if test -n "$ncolors" && test "$ncolors" -ge 8; then
    setnormal="$(tput sgr0)"
    setred="$(tput setaf 1)"
    setgreen="$(tput setaf 2)"
    setyellow="$(tput setaf 3)"
  fi
fi

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
    if [[ ${temp_pwd} -ef . ]]; then
      echo "${setgreen}===== RUNNING: ${setyellow}$@${setnormal}"
    else
      echo "${setgreen}===== IN ${setyellow}${temp_pwd}${setgreen}, RUNNING: ${setyellow}$@${setnormal}"
    fi
  fi

  cd $temp_pwd
  $@
  exitcode=$?
  if [ $exitcode -ne 0 ]; then
    echo
    echo "${setred}========== Aborting: Command failed. =========="
    echo "${setred}DIRECTORY: ${setyellow}${temp_pwd}"
    echo "${setred}COMMAND  : ${setyellow}$@"
    echo "${setred}EXIT CODE: ${setyellow}${exitcode}"
    echo "${setred}===============================================${setnormal}"
    exit 1
  fi
  cd $old_pwd
}

function cabal_install {
  cabal install --disable-library-profiling --force-reinstalls --global --prefix=$BUILD $@
}
