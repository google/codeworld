#!/bin/bash

# Copyright 2015 Google Inc. All rights reserved.
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

rm -rf $BUILD ~/.ghc ~/.ghcjs

mkdir $BUILD
mkdir $BUILD/downloads
mkdir $BUILD/bin

# Determine which package management tool is installed, and install
# necessary system packages.

if type yum > /dev/null 2> /dev/null
then
  echo Detected 'yum': Installing packages from there.
  echo

  # Update and install basic dependencies

  run . sudo yum update -y

  run . sudo yum install -y git
  run . sudo yum install -y zlib-devel
  run . sudo yum install -y ncurses-devel

  # Needed for GHC 7.8
  run . sudo yum install -y gcc
  run . sudo yum install -y gmp-devel

  # Needed for ghcjs-boot --dev
  run . sudo yum install -y patch
  run . sudo yum install -y autoconf
  run . sudo yum install -y automake

  # Needed for nodejs
  run . sudo yum install -y gcc-c++
  run . sudo yum install -y openssl-devel
elif type apt-get > /dev/null 2> /dev/null
then
  echo Detected 'apt-get': Installing packages from there.
  echo

  # Update and install basic dependencies

  run . sudo apt-get update -y

  run . sudo apt-get install -y git
  run . sudo apt-get install -y xz-utils
  run . sudo apt-get install -y psmisc

  run . sudo apt-get install -y zlib1g-dev
  run . sudo apt-get install -y libncurses5-dev

  # Needed for GHC 7.8
  run . sudo apt-get install -y make
  run . sudo apt-get install -y gcc
  run . sudo apt-get install -y libgmp-dev

  # Needed for ghcjs-boot --dev
  run . sudo apt-get install -y patch
  run . sudo apt-get install -y autoconf
  run . sudo apt-get install -y automake
  run . sudo apt-get install -y libtinfo-dev

  # Needed for nodejs
  run . sudo apt-get install -y g++
  run . sudo apt-get install -y openssl
elif type zypper > /dev/null 2> /dev/null
then
  echo Detected 'zypper': Installing packages from there.
  echo

  # Update and install basic dependencies

  run . sudo zypper -n refresh

  run . sudo zypper -n install git
  run . sudo zypper -n install bzip2
  run . sudo zypper -n install psmisc

  run . sudo zypper -n install zlib-devel
  run . sudo zypper -n install ncurses-devel

  # Needed for GHC 7.8
  run . sudo zypper -n install make
  run . sudo zypper -n install gcc
  run . sudo zypper -n install gmp-devel

  # Needed for ghcjs-boot --dev
  run . sudo zypper -n install patch
  run . sudo zypper -n install autoconf
  run . sudo zypper -n install automake

  # Needed for nodejs
  run . sudo zypper -n install gcc-c++
  run . sudo zypper -n install openssl
else
  echo "WARNING: Could not find package manager."
  echo "Make sure necessary packages are installed."
fi

# Choose the right GHC download
if /sbin/ldconfig -p | grep -q libgmp.so.10; then
  GHC_ARCH=`uname -m`-deb8-linux
elif /sbin/ldconfig -p | grep -q libgmp.so.3; then
  GHC_ARCH=`uname -m`-centos67-linux
else
  echo Sorry, but no supported libgmp is installed.
  exit 1
fi

# Install GHC, since it's required for GHCJS.

GHC_DIR=7.10.3
GHC_VERSION=7.10.3

run $DOWNLOADS               wget http://downloads.haskell.org/~ghc/$GHC_DIR/ghc-$GHC_VERSION-$GHC_ARCH.tar.xz
run $BUILD                   tar xf $DOWNLOADS/ghc-$GHC_VERSION-$GHC_ARCH.tar.xz
run $BUILD/ghc-$GHC_VERSION  ./configure --prefix=$BUILD
run $BUILD/ghc-$GHC_VERSION  make install
run $BUILD                   rm -rf ghc-$GHC_VERSION

# Install all the dependencies for cabal

run $DOWNLOADS                     wget https://www.haskell.org/cabal/release/cabal-install-1.22.6.0/cabal-install-1.22.6.0.tar.gz
run $BUILD                         tar xf $DOWNLOADS/cabal-install-1.22.6.0.tar.gz
run $BUILD/cabal-install-1.22.6.0  ./bootstrap.sh
run .                              cabal update
run $BUILD                         rm -rf cabal-install-1.22.6.0

# Fetch the prerequisites for GHCJS.

run .  cabal_install happy-1.19.5 alex-3.1.6

# Install GHCJS itself (https://github.com/ghcjs/ghcjs) and cabal install.

run $BUILD  cabal_install http://ghcjs.luite.com/master.tar.gz
run $BUILD  rm -rf ghcjs

# install node (necessary for ghcjs-boot)

NODEJS_VERSION=v4.2.3

run $DOWNLOADS                   wget https://nodejs.org/dist/$NODEJS_VERSION/node-$NODEJS_VERSION.tar.gz
run $BUILD                       tar xzf $DOWNLOADS/node-$NODEJS_VERSION.tar.gz
run $BUILD/node-$NODEJS_VERSION  ./configure --prefix=$BUILD
run $BUILD/node-$NODEJS_VERSION  make
run $BUILD/node-$NODEJS_VERSION  make install
run $BUILD                       rm -rf node-$NODEJS_VERSION

# Bootstrap ghcjs

run . ghcjs-boot --no-prof --no-haddock

# Install ghcjs-dom from hackage.

run $BUILD  git clone https://github.com/ghcjs/ghcjs-dom
run $BUILD  cabal_install --ghcjs  ./ghcjs-dom
run $BUILD  rm -rf ghcjs-dom

run $BUILD  rm -rf downloads
