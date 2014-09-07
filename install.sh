#!/bin/bash

echo "This script does not function properly yet!  Please follow the"
echo "instructions in the README file."

echo "Continue anyway?"
select yn in "Yes" "No"; do
    case $yn in
        Yes ) break;;
        No  ) exit;;
    esac
done

BUILD=$(pwd)/build
DOWNLOADS=$BUILD/downloads

mkdir $BUILD
mkdir $BUILD/downloads
mkdir $BUILD/bin

export PATH=$BUILD/bin:$PATH

# Determine which package management tool is installed, and install
# necessary system packages.

if type yum > /dev/null
then
  # Update and install basic dependencies

  sudo yum update -y

  sudo yum install -y git
  sudo yum install -y zlib-devel
  sudo yum install -y ncurses-devel

  # Needed for GHC 7.8
  sudo yum install -y gcc
  sudo yum install -y gmp-devel

  # Needed for nodejs
  sudo yum install -y gcc-c++
  sudo yum install -y openssl-devel

  # Needed for ghcjs-boot --dev
  sudo yum install -y patch
  sudo yum install -y autoconf
  sudo yum install -y automake

  # Choose the right GHC 7.8.2 download
  GHC_ARCH=x86_64-unknown-linux-centos65
elif type apt-get > /dev/null
then
  # Update and install basic dependencies

  sudo apt-get update -y

  sudo apt-get install -y git
  sudo apt-get install -y zlib1g-dev
  sudo apt-get install -y libncurses5-dev

  # Needed for GHC 7.8
  sudo apt-get install -y make
  sudo apt-get install -y gcc
  sudo apt-get install -y libgmp-dev

  # Needed for ghcjs-boot --dev
  sudo apt-get install -y patch
  sudo apt-get install -y autoconf
  sudo apt-get install -y automake
  sudo apt-get install -y nodejs-legacy

  # Needed for nodejs
  sudo apt-get install -y g++
  sudo apt-get install -y openssl

  # Choose the right GHC 7.8.2 download
  GHC_ARCH=x86_64-unknown-linux-deb7
else
  echo "WARNING: Could not find package manager."
  echo "Make sure necessary packages are installed."
fi

# install node (necessary for ghcjs-boot)

(cd $DOWNLOADS && wget http://nodejs.org/dist/v0.10.29/node-v0.10.29.tar.gz)
(cd $BUILD && tar -zxf $DOWNLOADS/node-v0.10.29.tar.gz)
(cd $BUILD/node-v0.10.29 && ./configure --prefix=$BUILD)
(cd $BUILD/node-v0.10.29 && make)
(cd $BUILD/node-v0.10.29 && make install)

# Install GHC 7.8, since it's required for GHCJS.

(cd $DOWNLOADS && wget http://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-$GHC_ARCH.tar.bz2)
(cd $BUILD && tar -xjf $DOWNLOADS/ghc-7.8.2-$GHC_ARCH.tar.bz2)
(cd $BUILD/ghc-7.8.2 && ./configure --prefix=$BUILD)
(cd $BUILD/ghc-7.8.2 && make install)

# Install all the dependencies for cabal

fromHackage() {
    (cd $DOWNLOADS && wget https://hackage.haskell.org/package/$1-$2/$1-$2.tar.gz)
    (cd $BUILD && tar -xzf $DOWNLOADS/$1-$2.tar.gz)
    (cd $BUILD/$1-$2 && runghc $3 configure --prefix=$BUILD)
    (cd $BUILD/$1-$2 && runghc $3 build)
    (cd $BUILD/$1-$2 && runghc $3 install --global)
}

fromHackage zlib 0.5.4.1 Setup.hs
fromHackage stm 2.4.3 Setup.hs
fromHackage random 1.0.1.1 Setup.hs
fromHackage mtl 2.1.3.1 Setup.hs
fromHackage text 1.1.1.3 Setup.lhs
fromHackage parsec 3.1.5  Setup.hs
fromHackage network 2.5.0.0 Setup.hs
fromHackage HTTP 4000.2.17 Setup.lhs

# Get a patched version of cabal (https://github.com/ghcjs/cabal) and
# then git checkout ghcjs to switch to the GHCJS branch, and finally
# cabal install both the Cabal and cabal-install packages.

fromLocal() {
    (cd $1 && runghc Setup.hs configure --prefix=$BUILD)
    (cd $1 && runghc Setup.hs build)
    (cd $1 && runghc Setup.hs install --global)
}

git clone -b ghcjs https://github.com/ghcjs/cabal.git $BUILD/cabal
fromLocal $BUILD/cabal/Cabal Setup.hs
fromLocal $BUILD/cabal/cabal-install Setup.hs

cabal update

# Get GHCJS itself (https://github.com/ghcjs/ghcjs) and cabal install.

cabal install --global --prefix=$BUILD alex
cabal install --global --prefix=$BUILD happy

git clone https://github.com/ghcjs/ghcjs-prim.git $BUILD/ghcjs-prim
(cd $BUILD/ghcjs-prim && cabal install --global --prefix=$BUILD)

git clone https://github.com/ghcjs/ghcjs.git $BUILD/ghcjs
(cd $BUILD/ghcjs && cabal install --global --prefix=$BUILD)

# Bootstrap ghcjs

ghcjs-boot --dev

# Check out ghcjs-dom (https://github.com/ghcjs/ghcjs-dom) and install it

git clone https://github.com/ghcjs/ghcjs-dom.git $BUILD/ghcjs-dom
(cd $BUILD/ghcjs-dom && cabal install --global --ghcjs --prefix=$BUILD)

# Check out ghcjs-canvas (https://github.com/ghcjs/ghcjs-canvas) and install it

git clone https://github.com/ghcjs/ghcjs-canvas $BUILD/ghcjs-canvas
(cd $BUILD/ghcjs-canvas && cabal install --global --ghcjs --prefix=$BUILD)

# Install the codeworld-base package

(cd codeworld-base &&  cabal install --global --ghcjs --prefix=$BUILD)
(cd codeworld-base &&  cabal haddock)

# Build and run the autocomplete generator.

(cd codeworld-autocomplete &&  cabal install --global --prefix=$BUILD --dependencies-only)
(cd codeworld-autocomplete &&  cabal build)
(cd codeworld-autocomplete &&  ./run.sh)

# Build codeworld-server from this project: cd codeworld-server && cabal build

(cd codeworld-server &&  cabal install --global --prefix=$BUILD --dependencies-only)
(cd codeworld-server &&  cabal build)

# Get a Google API key, and store it in web/clientId.txt.

echo MYGOOGLEAPIKEY > codeworld-server/web/clientId.txt

# Run the server: cd codeworld-server && ./run.sh 8080.

(cd codeworld-server && ./run.sh 8080)
