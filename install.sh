#!/bin/bash

function run {
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
}

BUILD=$(pwd)/build
DOWNLOADS=$BUILD/downloads

rm -rf $BUILD ~/.ghc ~/.cabal

mkdir $BUILD
mkdir $BUILD/downloads
mkdir $BUILD/bin

export PATH=$BUILD/bin:$PATH

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

  # Choose the right GHC 7.8.2 download
  GHC_ARCH=x86_64-unknown-linux-centos65
elif type apt-get > /dev/null 2> /dev/null
then
  echo Detected 'apt-get': Installing packages from there.
  echo

  # Update and install basic dependencies

  run . sudo apt-get update -y

  run . sudo apt-get install -y git
  run . sudo apt-get install -y bzip2
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
  run . sudo apt-get install -y nodejs-legacy

  # Needed for nodejs
  run . sudo apt-get install -y g++
  run . sudo apt-get install -y openssl

  # Choose the right GHC 7.8.2 download
  GHC_ARCH=x86_64-unknown-linux-deb7
else
  echo "WARNING: Could not find package manager."
  echo "Make sure necessary packages are installed."
fi

export PREFIX=$BUILD

# Install GHC 7.8.3, since it's required for GHCJS.

run $DOWNLOADS        wget http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-$GHC_ARCH.tar.bz2
run $BUILD            tar xjf $DOWNLOADS/ghc-7.8.3-$GHC_ARCH.tar.bz2
run $BUILD/ghc-7.8.3  ./configure --prefix=$BUILD
run $BUILD/ghc-7.8.3  make install

# Install all the dependencies for cabal

run $DOWNLOADS                     wget https://www.haskell.org/cabal/release/cabal-install-1.22.6.0/cabal-install-1.22.6.0.tar.gz
run $BUILD                         tar xzf $DOWNLOADS/cabal-install-1.22.6.0.tar.gz
run $BUILD/cabal-install-1.22.6.0  ./bootstrap.sh
run .                              cabal update

function cabal_install {
  cabal install --global --prefix=$BUILD --reorder-goals --max-backjumps=-1 $@
}

# Fetch the prerequisites for GHCJS.

run . cabal_install happy-1.19.5 alex-3.1.4

# Get GHCJS itself (https://github.com/ghcjs/ghcjs) and cabal install.

run $BUILD git clone https://github.com/ghcjs/ghcjs-prim.git
run $BUILD git clone https://github.com/ghcjs/ghcjs.git
run $BUILD cabal_install ./ghcjs ./ghcjs-prim

# install node (necessary for ghcjs-boot)

run $DOWNLOADS           wget http://nodejs.org/dist/v0.12.7/node-v0.12.7.tar.gz
run $BUILD               tar xzf $DOWNLOADS/node-v0.12.7.tar.gz
run $BUILD/node-v0.12.7  ./configure --prefix=$BUILD
run $BUILD/node-v0.12.7  make
run $BUILD/node-v0.12.7  make install

# Bootstrap ghcjs

run . ghcjs-boot --dev

# Install ghcjs-dom from hackage.

run $BUILD cabal_install --ghcjs ghcjs-dom

# Check out ghcjs-canvas and install it

run $BUILD  git clone https://github.com/ghcjs/ghcjs-canvas
run $BUILD  cabal_install --ghcjs --prefix=$BUILD ./ghcjs-canvas
