CodeWorld
=========

CodeWorld is an educational environment using a variant of Haskell.  It
provides a simple mathematical model for geometric figures, animations,
and interactive games.  The web-based programming environment supports
this educational mission with a zero-setup editor and compiler, easy
sharing and cloud storage of projects, and the ability to run programs
right in the web browser using GHCJS.

Other options include:

- The use of plain Haskell instead of the educational variant, to build
  programs which can either be on the web site with GHCJS, or compiled
  natively with the `codeworld-api` package and `blank-canvas`.
- A block-based programming interface for drag and drop programming
  following the same mathematical model.

Status
======

Google is distributing the code for CodeWorld, but CodeWorld is not an
official Google project, and Google provides no support for it.

Getting Started
===============

Just visit https://code.world to get started.

There is no need to download or install anything to use CodeWorld.  This
repository will be useful if you prefer to fork and modify the CodeWorld
environment, or contribute changes.

Discuss and Learn More
======================

To discuss and hear announcements about CodeWorld, subscribe to the mailing
list at https://groups.google.com/forum/#!forum/codeworld-discuss

The mailing list should be used to:
- Hear announcements about and discuss upcoming changes and features.
- Ask questions about using the system, and give feedback about your
  experiences.
- Share interesting ways of using the site, related classroom activities, and
  more.

To report bugs or file formal feature requests, try
https://github.com/google/codeworld/issues.

Contributing
============

There is a slight bit of paperwork involved in contributing to CodeWorld.  You'll need to
agree to a Contributor License Agreement.  See CONTRIBUTING.md for details.

Build and Deployment
====================

Building and running CodeWorld can be a lengthy process, but is automated using the
installation scripts in the root directory, which work on most forms of Linux, including
Debian, Ubuntu, RedHat, and CentOS.  The step by step instructions are as follows:

0. Read the caveats, explained below.
1. Change to the root directory of the project.
2. Run `./install.sh` to install GHC, GHCJS, and required libraries.
3. Run `./build.sh` to build CodeWorld itself.
4. Run `./run.sh` to start the server.

You can now access the CodeWorld system at http://localhost:8080.

Caveats
-------

### Leaky GHCJS Sandboxing ###

While the installation process installs most of its files inside `codeworld/build`, it does
clobber `~/.ghc`, `~/.ghcjs`, and `~/.cabal`.  I recommend that you run CodeWorld as a
dedicated user account to avoid causing problems for other Haskell installations.  If you
don't, note that you will lose your user package database.

See bug #4 for details.

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
programs in CodeWorld, it's usually safe to remove the swap file after running the
server for the first time.)
