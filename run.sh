#!/bin/bash

BUILD=$(pwd)/build
DOWNLOADS=$BUILD/downloads

export PATH=$BUILD/bin:$PATH

function cabal_install {
  cabal install --global --prefix=$BUILD --reorder-goals --max-backjumps=-1 $@
}

# Install the codeworld-base package

(cd codeworld-base && cabal_install --ghcjs)
(cd codeworld-base && cabal haddock)

# Build and run the autocomplete generator.

(cd codeworld-autocomplete &&  cabal_install --dependencies-only)
(cd codeworld-autocomplete &&  cabal build)
(cd codeworld-autocomplete &&  ./run.sh)

# Build codeworld-server from this project: cd codeworld-server && cabal build

(cd codeworld-server &&  cabal_install --dependencies-only)
(cd codeworld-server &&  cabal build)

# Run the server: cd codeworld-server && ./run.sh 8080.

(cd codeworld-server && ./run.sh 8080)
