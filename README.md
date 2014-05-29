CodeWorld
=========

CodeWorld is an educational web-based programming environment using a variant of Haskell.  It includes:

- A set of web-based tools for students to easily create their own computer drawings, animations, and games.
- A simple mathematical model of computer games, inspired by gloss.
- The ability for students to run and use their creations right in a web browser using GHCJS as a compiler.

Status
======

The project is not yet complete.  The core components are present: the standard library, the model and its implementation.  Programs must currently be built from the command line.  The web-based environment will come soon.

Build Instructions
==================

Building CodeWorld is, unfortunately, an involved process at the moment.  Here are the approximate step by step instructions to get an example working:

1. Install GHC 7.8, since it's required for GHCJS.
2. Get a patched version of cabal (https://github.com/ghcjs/cabal/tree/ghcjs) and `cabal install`.
3. Get GHCJS itself (https://github.com/ghcjs/ghcjs) and `cabal install`.
4. Run `ghcjs-boot --init`.
5. Check out ghcjs-dom (https://github.com/ghcjs/ghcjs-dom) and install it with `cabal install --ghcjs`.
6. Check out ghcjs-canvas (https://github.com/ghcjs/ghcjs-canvas), fix the definitions of textAlign and textBaseline in the obvious way, and install it with `cabal install --ghcjs`.
7. Check out codeworld (this repo), and install the codeworld-base package inside with `cabal install --ghcjs`.
8. Install warp-static: `cabal install warp-static`.
9. Change to the examples directory, and build and run: `./build Example4` and `./run Example4`
10. Visit `http://localhost:3000` to use the example program.

Feature Requests
================

This is a list of ideas for how to make this awesome.

* Editor: CodeMirror, compile, auto-complete, refactoring (rename, duplicate, ...).
* Documentation: browsable haddock, doc-on-hover.
* Workspace: save on server, project switcher, library projects.
* Social: browse public projects, share with friends, comment on others' projects.
* Collaboration: import libraries across users, joint projects.
* Debugging: point-pins, scale-zoom controls, time-step debugger, NOT a language debugger.
* Export: permalink, chrome app, Android, iOS (maybe impossible without jailbreak?).
* Lessons: guided help, gallery, quizzes, "helpers" (list comprehensions, etc.).
