CodeWorld
=========

[![Build Status](https://travis-ci.org/google/codeworld.svg?branch=master)](https://travis-ci.org/google/codeworld)

CodeWorld is an educational environment using Haskell. It provides a simple
mathematical model for geometric figures, animations, and interactive and
multi-player games.  The web-based programming environment supports this
educational mission with a zero-setup editor and compiler, easy sharing and
cloud storage of projects, and the ability to run programs right in the web
browser using GHCJS.

There are several variants of CodeWorld available:

- [CodeWorld](https://code.world/) uses an educational variant of the Haskell
  language and libraries, designed to support mathematics instruction.
- [CodeWorld Haskell](https://code.world/haskell) is built against standard
  Haskell instead of the educational variant, to build programs which can either
  be on the web site with GHCJS, or compiled natively with the `codeworld-api`
  package and `blank-canvas`.
- [CodeWorld Blocks](https://code.world/blocks) provides a drag-and-drop
  programming user interface for younger students to build programs with
  CodeWorld.  This interface still has some bugs, and isn't recommended for
  use.

Status
======

CodeWorld is stable and has been used in schools for years!  See
[the users page](https://github.com/google/codeworld/blob/master/docs/Users.md) for a partial list.
We're constantly improving the environment, though.  Breaking changes, when
necessary, are scheduled to occur between typical (U.S.) K-12 school
semesters, to minimize disruption of existing classes.

Google is distributing the code for CodeWorld, but CodeWorld is not an
official Google project, and Google provides no support for it.  Instead,
questions about the project or code should be asked to the
[codeworld-discuss mailing list](https://groups.google.com/forum/#!members/codeworld-discuss).
A student-friendly question and answer forum is also available at
http://help.code.world for questions about programs written *using* CodeWorld,
rather than questions about building or modifying CodeWorld itself.

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
agree to a Contributor License Agreement.  See [`CONTRIBUTING.md`](CONTRIBUTING.md) for details.

Build and Deployment
====================

Building and running CodeWorld can be a lengthy process, but is automated using the
installation scripts in the root directory, which work on most forms of Linux, including
Debian, Ubuntu, RedHat, and CentOS.  The step by step instructions are as follows:

0. Read the caveats, explained below.
1. Change to the root directory of the project.
2. Run `./install.sh` to set up the project.
3. Run `./run.sh` to start the server.

You can now access the CodeWorld system at http://localhost:8080.

If you make changes to CodeWorld, you can rebuild it without rebuilding the dependencies:

1. Change to the root directory of the project.
2. Run `./build.sh` to recompile just CodeWorld itself, using previously installed tools and libraries.
3. Run `./run.sh` to start the server.

### Docker

It's also possible to build and run the server using Docker.  This is not yet the
recommended way to develop with CodeWorld, but it could get there soon.

Commands to try for docker:

    sudo docker build -t codeworld https://github.com/google/codeworld.git
    sudo docker run -p 80:8080 -t codeworld

For now, the docker container has no way to access a client id, mount a shared NFS
drive, or other setup steps.  It will work, but it won't be complete.  In the
future, this should become the standard way to deploy CodeWorld.

### Stack

The `stack.yaml` in the project's root is present to partially support Intero and
[Travis CI](https://travis-ci.org/google/codeworld/).  On Travis, `codeworld-compiler`
tests do not run; `codeworld-base` and `funblocks-client` are not built or tested.

Building and running CodeWorld locally with Stack is unsupported, and in fact doesn't
work.  Stack cannot yet substitute for the shell scripts or docker usage above.

Caveats
-------

### Authentication

CodeWorld offers two modes of authentication or the ability to run with
authentication disabled with reduced functionality. The two methods provided
are as follows:

* [Google authentication](#google-auth): this method uses the Google API and
  Google accounts and is the mode of authentication enabled in the live
CodeWorld site; this allows CodeWorld to offload account and credential
management to a third party
* [Local authentication](#local-auth): this method uses a simple local database
  of account information and JWT-based stateless authentication in the browser;
this is useful for applications where minimal external dependencies is required

Running CodeWorld in one of these two modes allows users to save and manage
their projects and folders. With no authentication enabled, users are able to
write, build and run code but lose the ability to save and manage projects and
folders.

#### <a name="google-auth"></a> Google authentication

For Google authentication to work, you will need to obtain a Google API key and
store it in `web/clientId.txt`.

To get a Google API key for your CodeWorld installation, please consult the
following resources:

* [Creating a project](https://cloud.google.com/resource-manager/docs/creating-managing-projects)
* [Creating an OAuth client ID](https://support.google.com/cloud/answer/6158849?hl=en)

Once you have a Google API key, copy and paste it into `web/clientId.txt`. A running
CodeWorld instance will immediately pick up changes to this file.

In general, the Google authentication system will be the easiest system to
maintain since no local password stores are required. This is the mechanism
used by the official, live version of CodeWorld.

#### <a name="local-auth"></a> Local authentication

For applications in which external dependencies, such as Google accounts, are
not acceptable, we provide a simple local authentication system:

* Uses a SQLite3 database
* Uses good security practices by storing only BCrypt hashes of passwords
* Uses JWT-based stateless authentication

This provides a local authentication system with very similar workflows to
Google authentication (i.e. stateless client-side sessions). Currently, no
web-based administrative interface is provided. Instead, you can use the
`codeworld-auth` CLI tool to manage accounts.

The local authentication system may be useful for situations where an
instructor cannot reasonably expect all students to have a valid Google account
and in which the instructor is willing to deploy a local CodeWorld stack.

##### Create account database

Local authentication will be enabled if a `codeworld-auth.db` file is present
in the application's root directory. To create this database, run the following
from the root of the Git repository:

```
build/bin/codeworld-auth init-accounts -d codeworld-auth.db
```

This will create an empty account database with no accounts.

##### Create one or more user accounts

Assuming you have already created an account database as described above, you
can create a new account as follows:

```
build/bin/codeworld-auth create-account -d codeworld-auth.db johndoe Expired
```

This will create a new account with user ID `johndoe` with a randomly generated
password. The account will be set to "Expired" which means the user will be
prompted to enter a new password at next sign-in time.

Other subcommands are provided for updating and deleting accounts etc. For
help:

```
build/bin/codeworld-auth --help
```

##### Create a JWT secret

To use local authentication, you will also need to generate a JWT secret stored
in a file named `codeworld-auth.txt`. This is used to sign JWT tokens passed
back and forth between the server and the browser. From the Git repository's
root directory, run the following command:

```
build/bin/codeworld-auth generate-secret -s codeworld-auth.txt
```

This will generate a new random JWT signing key. The server should not expose
this secret to external users.

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
