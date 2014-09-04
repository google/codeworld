#!/bin/sh

echo "This script does not function properly yet!  Please follow the"
echo "instructions in the README file."

# exit 1

# Determine which package management tool is installed.

BUILD=$(pwd)/build
DOWNLOADS=$BUILD/downloads

mkdir $BUILD
mkdir $BUILD/downloads
mkdir $BUILD/bin

export PATH=$BUILD/bin:$PATH

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

  # Needed for ghcjs-boot --dev

  sudo yum install -y patch
  sudo yum install -y autoconf
  sudo yum install -y automake

  # Needed for nodejs
  sudo yum install -y gcc-c++
  sudo yum install -y openssl-devel

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

  # Needed for nodejs
  sudo apt-get install -y nodejs
  alias node=nodejs

  # Choose the right GHC 7.8.2 download
  GHC_ARCH=x86_64-unknown-linux-deb7
else
  echo "WARNING: Could not find package manager."
  echo "Make sure necessary packages are installed."
fi

# Install GHC 7.8, since it's required for GHCJS.

(cd $DOWNLOADS && wget http://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-$GHC_ARCH.tar.bz2)
(cd $BUILD && tar -xjf $DOWNLOADS/ghc-7.8.2-$GHC_ARCH.tar.bz2)
(cd $BUILD/ghc-7.8.2 && ./configure --prefix=$BUILD)
(cd $BUILD/ghc-7.8.2 && make install)

# install node (necessary for ghcjs-boot)

(cd $DOWNLOADS && wget http://nodejs.org/dist/v0.10.29/node-v0.10.29.tar.gz)
(cd $BUILD && tar -zxf $DOWNLOADS/node-v0.10.29.tar.gz)
(cd $BUILD/node-v0.10.29 && ./configure --prefix=$BUILD)
(cd $BUILD/node-v0.10.29 && make)
(cd $BUILD/node-v0.10.29 && make install)

# Install all the dependencies for cabal

fromHackage() {
    (cd $DOWNLOADS && wget https://hackage.haskell.org/package/$1-$2/$1-$2.tar.gz)
    (cd $BUILD && tar -xzf $DOWNLOADS/$1-$2.tar.gz)
    (cd $BUILD/$1-$2 && runghc $3 configure --prefix=$BUILD)
    (cd $BUILD/$1-$2 && runghc $3 build)
    (cd $BUILD/$1-$2 && runghc $3 install)
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
    (cd $1 && runghc $2 configure --prefix=$BUILD)
    (cd $1 && runghc $2 build)
    (cd $1 && runghc $2 install)
}

git clone -b ghcjs https://github.com/ghcjs/cabal.git $BUILD/cabal
(cd $BUILD/cabal && git checkout f70ed6277942c74bdd68f63e2b0694d57dcb8606) # Current ghcjs/HEAD doesn't build
fromLocal $BUILD/cabal/Cabal Setup.hs
fromLocal $BUILD/cabal/cabal-install Setup.hs

cabal update

# Get GHCJS itself (https://github.com/ghcjs/ghcjs) and cabal install.

cabal install --prefix=$BUILD alex
cabal install --prefix=$BUILD happy

git clone https://github.com/ghcjs/ghcjs-prim.git $BUILD/ghcjs-prim
(cd $BUILD/ghcjs-prim && git checkout 54d2ce21d713fe052dcad8e815e13c13585a4262)
(cd $BUILD/ghcjs-prim && cabal install --prefix=$BUILD)

git clone https://github.com/ghcjs/ghcjs.git $BUILD/ghcjs
(cd $BUILD/ghcjs && git checkout e7d77288517e202708187fe6bcca2eead7aefa6c)
(cd $BUILD/ghcjs && cabal install --prefix=$BUILD)

# Bootstrap ghcjs

ghcjs-boot --dev

# Check out ghcjs-dom (https://github.com/ghcjs/ghcjs-dom) and install it

git clone https://github.com/ghcjs/ghcjs-dom.git $BUILD/ghcjs-dom
(cd $BUILD/ghcjs-dom && git checkout 1fb18971dff6a793de27d95b4561411ff0f9c722)
(cd $BUILD/ghcjs-dom && cabal install --ghcjs --prefix=$BUILD)

# Check out ghcjs-canvas (https://github.com/ghcjs/ghcjs-canvas) and install it

git clone https://github.com/ghcjs/ghcjs-canvas $BUILD/ghcjs-canvas
(cd $BUILD/ghcjs-canvas && git checkout 73a09bfc538b61f05299f58dedb5f3010437efcc)
(cd $BUILD/ghcjs-canvas && cabal install --ghcjs --prefix=$BUILD)

# Install the codeworld-base package

(cd codeworld-base &&  cabal install --ghcjs --prefix=$BUILD)

# Build and run the autocomplete generator.

(cd codeworld-autocomplete &&  cabal install --prefix=$BUILD --dependencies-only)
(cd codeworld-autocomplete &&  ./run.sh)

# Build codeworld-server from this project: cd codeworld-server && cabal build

(cd codeworld-server &&  cabal install --prefix=$BUILD --dependencies-only)
(cd codeworld-server &&  cabal build)

# Get a Google API key, and store it in web/clientId.txt.

echo MYGOOGLEAPIKEY > codeworld-server/web/clientId.txt

# Run the server: cd codeworld-server && ./run.sh 8080.

(cd codeworld-server && ./run.sh 8080)
