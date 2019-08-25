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

mkdir -p $BUILD/downloads
mkdir -p $BUILD/bin
mkdir -p $BUILD/progress

# Determine which package management tool is installed, and install
# necessary system packages.

if [ ! -f $BUILD/progress/system-pkgs ]; then
  if type yum > /dev/null 2> /dev/null
  then
    echo Detected 'yum': Installing packages from there.
    echo

    # Update and install basic dependencies

    run . sudo yum update -y

    run . sudo yum install -y git
    run . sudo yum install -y curl
    run . sudo yum install -y wget
    run . sudo yum install -y bzip2
    run . sudo yum install -y zlib-devel
    run . sudo yum install -y ncurses-devel

    # Needed for GHC
    run . sudo yum install -y gcc
    run . sudo yum install -y gcc-c++
    run . sudo yum install -y gmp-devel

    # Needed for GHCJS
    run --quiet . curl --silent --location https://rpm.nodesource.com/setup_8.x | run . sudo bash -
    run . sudo yum install -y nodejs

    # Needed for ghcjs-boot --dev
    run . sudo yum install -y patch
    run . sudo yum install -y autoconf
    run . sudo yum install -y automake

    # Needed for codeworld-auth
    run . sudo yum install -y openssl-devel
  elif type apt-get > /dev/null 2> /dev/null
  then
    echo Detected 'apt-get': Installing packages from there.
    echo

    # Update and install basic dependencies

    run . sudo apt-get update -y

    run . sudo apt-get install -y git
    run . sudo apt-get install -y curl
    run . sudo apt-get install -y wget
    run . sudo apt-get install -y bzip2
    run . sudo apt-get install -y xz-utils
    run . sudo apt-get install -y psmisc

    run . sudo apt-get install -y zlib1g-dev
    run . sudo apt-get install -y libncurses5-dev

    # Needed for GHC
    run . sudo apt-get install -y make
    run . sudo apt-get install -y gcc
    run . sudo apt-get install -y g++
    run . sudo apt-get install -y libgmp-dev

    # Needed for GHCJS
    run . sudo apt-get install -y gnupg
    run --quiet . curl -sL https://deb.nodesource.com/setup_8.x | run . sudo -E bash -
    run . sudo apt-get install -y nodejs

    # Needed for ghcjs-boot --dev
    run . sudo apt-get install -y patch
    run . sudo apt-get install -y autoconf
    run . sudo apt-get install -y automake
    run . sudo apt-get install -y libtinfo-dev

    # Needed for codeworld-auth
    run . sudo apt-get install -y libssl-dev
  elif type zypper > /dev/null 2> /dev/null
  then
    echo Detected 'zypper': Installing packages from there.
    echo

    # Update and install basic dependencies

    run . sudo zypper -n refresh

    run . sudo zypper -n install git
    run . sudo zypper -n install curl
    run . sudo zypper -n install wget
    run . sudo zypper -n install bzip2
    run . sudo zypper -n install psmisc

    run . sudo zypper -n install zlib-devel
    run . sudo zypper -n install ncurses-devel

    # Needed for GHC
    run . sudo zypper -n install make
    run . sudo zypper -n install gcc
    run . sudo zypper -n install gmp-devel

    # Needed for GHCJS
    run . sudo zypper -n install nodejs6

    # Needed for ghcjs-boot --dev
    run . sudo zypper -n install patch
    run . sudo zypper -n install autoconf
    run . sudo zypper -n install automake

    # Needed for codeworld-auth
    run . sudo zypper -n install libopenssl-devel
  elif type brew > /dev/null 2> /dev/null
  then
    echo Detected 'brew': Installing packages from there.
    echo

    # install missing packages -- don't try to upgrade already installed packages
    function brew_install {
      if ! brew ls $1 > /dev/null 2> /dev/null
      then
        run . brew install $1
      fi
    }

    # Update and install basic dependencies
    xcode-select --install

    brew_install git
    brew_install curl
    brew_install wget
    brew_install bzip2

    # Needed for GHC
    brew_install make
    brew_install gcc

    # Needed for GHCJS
    brew_install node@6
    brew_install gnu-tar

    # Needed for ghcjs-boot --dev
    brew_install autoconf
    brew_install automake

    # Needed for codeworld-auth
    brew_install openssl
  else
    echo "WARNING: Could not find package manager."
    echo "Make sure necessary packages are installed."
  fi

  touch $BUILD/progress/system-pkgs
fi

# Install ghcup, a minimal tool for installing GHC.

if [ ! -f $BUILD/progress/ghcup ]; then
  run .                  mkdir -p $BUILD/.ghcup/bin
  run $BUILD/.ghcup/bin  wget https://gitlab.haskell.org/haskell/ghcup/raw/master/ghcup
  run .                  chmod +x $BUILD/.ghcup/bin/ghcup

  touch $BUILD/progress/ghcup
fi

# Install GHC.

if [ ! -f $BUILD/progress/ghc ]; then
  run .                  ghcup install 8.6.5
  run .                  ghcup set 8.6.5

  touch $BUILD/progress/ghc
fi

if [ ! -f $BUILD/progress/cabal-install ]; then
  run .                  ghcup install-cabal
  run .                  cabal update

  touch $BUILD/progress/cabal-install
fi

# Install GHCJS itself (https://github.com/ghcjs/ghcjs), which depends on happy and alex.

function fix_libexec_binary {
  # Work-around for https://github.com/ghcjs/ghcjs/issues/740
  # Should be run from $BUILD/bin

  if [ ! -e $BUILD/bin/$1 ]; then
    ln -s $(dirname $(dirname $(readlink $1)))/libexec/$1 $1-new
    mv $1-new $1
  fi
}

if [ ! -f $BUILD/progress/ghcjs ]; then
  run .            cabal v2-install happy-1.19.9 alex --symlink-bindir=$BUILD/bin
  run $BUILD       rm -rf ghcjs
  run $BUILD       git clone --branch ghc-8.6 --single-branch https://github.com/ghcjs/ghcjs.git
  run $BUILD/ghcjs git submodule update --init --recursive
  run .            patch -p0 -u -d $BUILD < ghc-artifacts/ghcjs-8.6-default-main.patch
  run $BUILD/ghcjs ./utils/makePackages.sh
  run $BUILD/ghcjs cabal v2-install . --symlink-bindir=$BUILD/bin -j1 --disable-documentation --overwrite-policy=always

  run $BUILD/bin   fix_libexec_binary ghcjs-boot
  run $BUILD/bin   fix_libexec_binary ghcjs-run
  run $BUILD/bin   fix_libexec_binary ghcjs-dumparchive

  touch $BUILD/progress/ghcjs
fi

if [ ! -f $BUILD/progress/ghcjs-boot ]; then
  run $BUILD/ghcjs  ghcjs-boot -j$NPROC --no-prof --no-haddock -s lib/boot
  touch $BUILD/progress/ghcjs-boot

  run .  cabal_install --ghcjs ./build/ghcjs/lib/ghc-api-ghcjs \
                              ./build/ghcjs/lib/template-haskell-ghcjs \
                              ./build/ghcjs/lib/ghci-ghcjs
fi

# Install tools to build CodeMirror editor.

if [ ! -f $BUILD/progress/codemirror ]; then
  run $BUILD            rm -rf $BUILD/CodeMirror
  run $BUILD            git clone https://github.com/codemirror/CodeMirror.git
  run $BUILD/CodeMirror git checkout tags/5.43.0
  run $BUILD/CodeMirror npm install
  run $BUILD/CodeMirror npm install -s uglify-js https://github.com/angelozerr/CodeMirror-Extension

  touch $BUILD/progress/codemirror
fi

# Fetch third_party/blockly submodule
run . git submodule init
run . git submodule update

# Go ahead and run a first build, which installs more local packages.
./build.sh
