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

# Choose the right GHC download
MACHINE="$(uname -m)"
case "${MACHINE}" in
  i386)   GHC_CPU=i386;;
  i686)   GHC_CPU=i386;;
  x86_64) GHC_CPU=x86_64;;
  amd7)   GHC_CPU=x86_64;;
  *) >&2 echo "Unrecognized machine: ${MACHINE}"; exit 1;;
esac

set +e
result=$(/sbin/ldconfig -p 2> /dev/null)
set -e
if [[ $result = *libgmp.so.10* ]]; then
  GHC_ARCH="${GHC_CPU}-deb9-linux"
elif [[ $result = *libgmp.so.3* ]]; then
  GHC_ARCH="${GHC_CPU}-centos7-linux"
else
  set +e
  result=$(uname 2> /dev/null)
  set -e
  if [[ $result = *Darwin* ]]; then
    GHC_ARCH="${GHC_CPU}-apple-darwin"
  else
    echo Sorry, but no supported libgmp is installed.
    exit 1
  fi
fi

GHC_DIR=8.6.5
GHC_VERSION=8.6.5

# Install a precompiled GHC to bootstrap itself.

if [ ! -f $BUILD/progress/bootstrap-ghc ]; then
  run $DOWNLOADS               wget https://downloads.haskell.org/~ghc/$GHC_DIR/ghc-$GHC_VERSION-$GHC_ARCH.tar.xz
  run $BUILD                   rm -rf ghc-$GHC_VERSION
  run $BUILD                   tar xf $DOWNLOADS/ghc-$GHC_VERSION-$GHC_ARCH.tar.xz
  run $BUILD/ghc-$GHC_VERSION  ./configure --prefix=$BUILD
  run $BUILD/ghc-$GHC_VERSION  make install
  run $BUILD                   rm -rf ghc-$GHC_VERSION
  run $DOWNLOADS               rm -rf *

  touch $BUILD/progress/bootstrap-ghc
fi

# Now install the patched GHC, built from source.

if [ ! -f $BUILD/progress/patched-ghc ]; then
  run $DOWNLOADS               wget https://downloads.haskell.org/~ghc/$GHC_DIR/ghc-$GHC_VERSION-src.tar.xz
  run $BUILD                   rm -rf ghc-$GHC_VERSION
  run $BUILD                   tar xf $DOWNLOADS/ghc-$GHC_VERSION-src.tar.xz
  run .                        patch -p0 -u -d $BUILD < ghc-artifacts/ghc-$GHC_VERSION-default-main.patch
  run .                        cp ghc-artifacts/build.mk $BUILD/ghc-$GHC_VERSION/mk/build.mk
  run $BUILD/ghc-$GHC_VERSION  ./configure --prefix=$BUILD
  run $BUILD/ghc-$GHC_VERSION  make
  run $BUILD/ghc-$GHC_VERSION  make install
  run $BUILD                   rm -rf ghc-$GHC_VERSION
  run $DOWNLOADS               rm -rf *

  touch $BUILD/progress/patched-ghc
fi

# Install cabal-install

if [ ! -f $BUILD/progress/cabal-install ]; then
  run $DOWNLOADS                     wget https://www.haskell.org/cabal/release/cabal-install-2.4.1.0/cabal-install-2.4.1.0.tar.gz
  run $BUILD                         rm -rf cabal-install-2.4.1.0
  run $BUILD                         tar xf $DOWNLOADS/cabal-install-2.4.1.0.tar.gz
  EXTRA_CONFIGURE_OPTS="" run $BUILD/cabal-install-2.4.1.0 ./bootstrap.sh
  run .                              cabal update
  run $BUILD                         rm -rf cabal-install-2.4.1.0
  run $DOWNLOADS                     rm -rf *

  touch $BUILD/progress/cabal-install
fi

# Install GHCJS itself (https://github.com/ghcjs/ghcjs), which depends on happy and alex.

if [ ! -f $BUILD/progress/ghcjs ]; then
  run .            cabal_install happy-1.19.9 alex
  run $BUILD       rm -rf ghcjs
  run $BUILD       git clone --branch ghc-8.6 --single-branch https://github.com/ghcjs/ghcjs.git
  run $BUILD/ghcjs git submodule update --init --recursive
  run .            patch -p0 -u -d $BUILD < ghc-artifacts/ghcjs-8.6-default-main.patch
  run $BUILD/ghcjs ./utils/makePackages.sh
  run $BUILD/ghcjs cabal v2-install . --symlink-bindir=$BUILD/bin -j1 --disable-documentation --overwrite-policy=always

  touch $BUILD/progress/ghcjs
fi

if [ ! -f $BUILD/progress/ghcjs-boot ]; then
  run $BUILD/ghcjs  ghcjs-boot --no-prof --no-haddock -s lib/boot
  touch $BUILD/progress/ghcjs-boot
fi

# Install tools to build CodeMirror editor.

if [ ! -f $BUILD/progress/codemirror ]; then
  run $BUILD            rm -rf $BUILD/CodeMirror
  run $BUILD            git clone https://github.com/codemirror/CodeMirror.git
  run $BUILD/CodeMirror git checkout tags/5.43.0
  run $BUILD/CodeMirror npm install
  run $BUILD/CodeMirror npm install -s uglify-js git+ssh://git@github.com:angelozerr/CodeMirror-Extension.git

  touch $BUILD/progress/codemirror
fi

# Go ahead and run a first build, which installs more local packages.
./build.sh
