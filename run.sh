#!/bin/bash

BUILD=$(pwd)/build
DOWNLOADS=$BUILD/downloads

export PATH=$BUILD/bin:$PATH

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

# Run the server: cd codeworld-server && ./run.sh 8080.

(cd codeworld-server && ./run.sh 8080)
