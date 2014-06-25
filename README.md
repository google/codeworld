CodeWorld
=========

CodeWorld is an educational web-based programming environment using a variant of Haskell.  It includes:

- A set of web-based tools for students to easily create their own computer drawings, animations, and games.
- A simple mathematical model of computer games, inspired by gloss.
- The ability for students to run and use their creations right in a web browser using GHCJS as a compiler.

Status
======

CodeWorld is an unofficial project.  It is not an official Google project, and Google
provides no support for it.

Discuss and Learn More
======================

To discuss and hear announcements about CodeWorld, subscribe to the mailing list at
https://groups.google.com/forum/#!forum/codeworld-discuss

The mailing list should be used to:
- Hear announcements about and discuss upcoming changes and features.
- Ask questions about using the system, and give feedback about your experiences.
- Share interesting ways of using the site, related classroom activities, and more.

To report bugs or file formal feature requests, try https://github.com/google/codeworld/issues --
but it would be great if you discuss your ideas for new features before filing a feature request.

Contributing
============

There is a slight bit of paperwork involved in contributing to CodeWorld.  You'll need to
agree to a Contributor License Agreement.  See CONTRIBUTING.md for details.

Build and Deployment
====================

Building CodeWorld is, unfortunately, an involved process at the moment.  Here are the
approximate step by step instructions to get an example working:

1. Install GHC 7.8, since it's required for GHCJS.
2. Get a patched version of cabal (https://github.com/ghcjs/cabal) and
   then `git checkout ghcjs` to switch to the GHCJS branch, and finally
   `cabal install` both the Cabal and cabal-install packages.
3. Get GHCJS itself (https://github.com/ghcjs/ghcjs) and `cabal install`.
4. Run `ghcjs-boot --init`.
5. Check out ghcjs-dom (https://github.com/ghcjs/ghcjs-dom) and install it with `cabal install --ghcjs`.
6. Check out ghcjs-canvas (https://github.com/ghcjs/ghcjs-canvas) and install it with `cabal install --ghcjs`.
7. Install the codeworld-base package from this project: `cd codeworld-base && cabal install --ghcjs`.
8. Build codeworld-server from this project: `cd codeworld-server && cabal build`
9. Get a Google API key, and store it in web/clientId.txt.
10. Run the server: `cd codeworld-server && ./run.sh 8080`.

You can now access the CodeWorld system at http://localhost:8080.
