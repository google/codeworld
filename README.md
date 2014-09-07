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

Building and running CodeWorld can be a lengthy process.  Most of it is automated using
the installation scripts in the root directory.  The step by step instructions are as
follows:

0. Read the caveats, explained below.
1. Change to the root directory of the project.
2. Run ./install.sh, to install GHC, GHCJS, and required libraries.
3. Run ./run.sh to build and run CodeWorld itself.

You can now access the CodeWorld system at http://localhost:8080.

Caveats
-------

### Google API Key ###

CodeWorld allows users to authenticate using a Google account, and save
their projects.  For this feature to work, you need to obtain a Google API key, and store
it in codeworld-server/web/clientId.txt.  If you don't do this, the sign-in and save
features will not function correctly, but the rest of the site will be usable.

### Swap Space ###

If you are installing CodeWorld on a virtual server, be aware that the default
RAM on these servers is often not sufficient for GHC.  CodeWorld needs to compile very
large Haskell projects during its installation.  The following should be sufficient to
resolve any out-of-memory problems you encounter:

    $ sudo dd if=/dev/zero of=/swap bs=1024 count=2097152
    $ sudo mkswap /swap
    $ sudo swapon /swap

This creates a 2 GB swap file to increase available virtual memory.  Installation with
a swap file may be slow, but it will succeed.  (Unless you intend to write very large
programs in CodeWorld, it's usually safe to remove the swap file before running the
server.)
