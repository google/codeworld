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

rm -rf $BUILD ~/.ghc ~/.ghcjs

mkdir $BUILD
mkdir $BUILD/downloads
mkdir $BUILD/bin

export PATH=$BUILD/bin:$PATH
export LANG=${LANG:-C.UTF-8}

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

export PREFIX=$BUILD

# Choose the right GHC download
if /sbin/ldconfig -p | grep -q libgmp.so.10; then
  GHC_ARCH=x86_64-unknown-linux-deb7
elif /sbin/ldconfig -p | grep -q libgmp.so.3; then
  GHC_ARCH=x86_64-unknown-linux-centos65
else
  echo Sorry, but no supported libgmp is installed.
  exit 1
fi

# Install GHC, since it's required for GHCJS.

GHC_VERSION=7.10.2-rc2
GHC_REAL_VERSION=7.10.1.20150630

run $DOWNLOADS                    wget http://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_REAL_VERSION-$GHC_ARCH.tar.xz
run $BUILD                        tar xf $DOWNLOADS/ghc-$GHC_REAL_VERSION-$GHC_ARCH.tar.xz
run $BUILD/ghc-$GHC_REAL_VERSION  ./configure --prefix=$BUILD
run $BUILD/ghc-$GHC_REAL_VERSION  make install

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

run $BUILD git clone -b improved-base https://github.com/ghcjs/ghcjs-prim.git
run $BUILD git clone -b improved-base https://github.com/ghcjs/ghcjs.git
run $BUILD cabal_install ./ghcjs ./ghcjs-prim

# install node (necessary for ghcjs-boot)

run $DOWNLOADS           wget http://nodejs.org/dist/v0.12.7/node-v0.12.7.tar.gz
run $BUILD               tar xzf $DOWNLOADS/node-v0.12.7.tar.gz
run $BUILD/node-v0.12.7  ./configure --prefix=$BUILD
run $BUILD/node-v0.12.7  make
run $BUILD/node-v0.12.7  make install

# Bootstrap ghcjs

run . ghcjs-boot --dev --no-prof --no-haddock --ghcjs-boot-dev-branch improved-base --shims-dev-branch improved-base

# Install ghcjs-dom from hackage.

run $BUILD  git clone https://github.com/ghcjs/ghcjs-dom
run $BUILD  cabal_install --ghcjs --prefix=$BUILD ./ghcjs-dom
