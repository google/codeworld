CodeWorld
=========

CodeWorld is an educational web-based programming environment using a variant of Haskell.  It includes:

- A set of web-based tools for students to easily create their own computer drawings, animations, and games.
- A simple mathematical model of computer games, inspired by gloss.
- The ability for students to run and use their creations right in a web browser using GHCJS as a compiler.

Status
======

This is currently a proof of concept.  The core components are present: the web-based development environment, the standard library, the model, and its implementation.  The current version has been tested in recent versions of Chrome, but appears to not work in other browsers.  Many basic convenience functions (even important ones, like saving the program) are not implemented.  Use at your own risk.

Build Instructions
==================

Building CodeWorld is, unfortunately, an involved process at the moment.  Here are the approximate step by step instructions to get an example working:

1. Install GHC 7.8, since it's required for GHCJS.
2. Get a patched version of cabal (https://github.com/ghcjs/cabal/tree/ghcjs) and `cabal install`.
3. Get GHCJS itself (https://github.com/ghcjs/ghcjs) and `cabal install`.
4. Run `ghcjs-boot --init`.
5. Check out ghcjs-dom (https://github.com/ghcjs/ghcjs-dom) and install it with `cabal install --ghcjs`.
6. Check out ghcjs-canvas (https://github.com/ghcjs/ghcjs-canvas) and install it with `cabal install --ghcjs`.
8. Install the codeworld-base package from this project: `cd codeworld-base && cabal install --ghcjs`.
9. Build codeworld-server from this project: `cd codeworld-server && cabal build`
10. Run the server: `cd codeworld-server && ./run.sh`.

You can now access the CodeWorld system at http://localhost:8080.

Feature Requests
================

This is a list of ideas for how to make this awesome.

* Language: Remove the Eq type class
* Editor: Auto-complete, refactoring (rename, duplicate, ...).
* Documentation: Fully document haddock, custom haddock theme, doc-on-hover, more browsable examples.
* Workspace: project switcher, better example browser, library projects.
* Social: browse public projects, share with friends, comment on others' projects.
* Collaboration: import libraries across users, joint projects.
* Debugging: Show runtime exceptions, click-jump to error location, point-pins, scale-zoom controls,
             time-step debugger, NOT a language debugger.
* Export: Android, iOS (maybe impossible without jailbreak?), chrome app? flash?.
* Lessons: guided help, gallery, quizzes, "helpers" (list comprehensions, etc.).

Bugs
====

- Document dirty state should be tracked
- MouseDrag event doesn't work