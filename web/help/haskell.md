Welcome to CodeWorld Haskell!
=============================

You can use most of the Haskell programming language (not including the FFI or
Template Haskell) using this web site.  For an introduction to Haskell, see
http://haskell.org

API Documentation
-----------------

The CodeWorld graphics and interaction APIs are defined in the `CodeWorld`
module.  The generated API reference for this module is here.

* [CodeWorld API Reference][1]

Building Locally
----------------

You can build and run your programs locally, as well.  To compile a program on
your own computer, you'll need to install GHC, and then run:

    cabal install codeworld-api

This will install `codeworld-api` and its dependencies.  You can then download
your program, and compile it with GHC.  The resulting program with start a web
browser, and then print a URL to visit to run your program.

[1]: ./doc-haskell/CodeWorld.html "API Documentation"
