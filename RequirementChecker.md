The CodeWorld Requirement Checker
=================================

WARNING: This is an experimental feature.  Everything in this
document is subject to change!  You have been warned.

The CodeWorld requirement checker provides a language for embedding
exercise requirements into Haskell source files, so that they can be
checked and the results automatically reported.

High-level Overview
-------------------

Requirements checking is an additional step run as part of the
CodeWorld compilation process.  It compares the submitted code against
a set of requirements embedded into source comments, and produces a
block of output reporting the result of each requirement.  The
CodeWorld environment recognizes this block of output, and formats it
nicely for the user to check their work.

The input format inside the source code consists of REQUIRES or
XREQUIRES comments, the high-level format of which is described in the
next section.  The output from the compiler consists of a
`:: REQUIREMENTS ::` block.  Finally, the most complex piece
is the requirement specification language, described at the end.

Requirement Comments
--------------------

There are two ways to embed requirements into a source file: REQUIRES
comments, or XREQUIRES comments.  You will typically start with the
first during development, and then optionally switch to the second
when you're done.

A REQUIRES comment looks like this:

    {-
        REQUIRES "Some user-readable description"
        <<list of machine-readable conditions>>
    -}

The user-readable description is an arbitrary string, which is used
in the output to provide a section header explaining what went wrong.
The machine-readable conditions describe the formal requirements that
are varified.  Details on that language are below.

An XREQUIRES comment is an obfuscated variation on REQUIRES.  Instead
of a single requirement, it contains a base64-encoded and gzipped list
of requirements.  It looks like this:

    {-
        XREQUIRES
        eJyNkE9LxDAQxb/KkNMKQWxtd/Wo0IW96S56sUJmm+km0CQlyeKfT2+oDQq6
        sKcZeL957zEvbNs8Pm22zQ5aJkbvDh6N4CCkiyFNtHLaBQTljoME6yLsCYyT
        utckL1vWWgAwGDtFoXkfqYskF7MTh7paVqvy4l8o+XIoyroolieBwOGmXl3d
        1olg/HfbO+iPtovaWRB9yiIBB7LkcdCfFCAqgs4Zk+QRYyRvc1eFYafNONAD
        prOwmI7/2D+j17gfkhPGyYzSK9ATSOq1JQlvOqpJyOm5Ts6ZwfuP9Sx8JxUc
        cuJpqDwHuj4Hqn4g9voF6VGoEg==
    -}

The purpose of an XREQUIRES comment is to embed requirements in a way
that does not give away the solution to someone who casually glances
at the comment.  It is merely obfuscation rather than encryption, but
still, sometimes it is valuable to help honest learners avoid peeking
at the answers by accident.  One writes an XREQUIRES comment by first
writing REQUIRES comments, and then copying the obfuscated form from
the compiler output when running the program with unobfuscated
requirements.

Requirements Output Block
-------------------------

A typical requirements output block looks something like this:

              :: REQUIREMENTS ::
    Obfuscated:
        XREQUIRES
        eJyNkE9LxDAQxb/KkNMKQWxtd/Wo0IW96S56sUJmm+km0CQlyeKfT2+oDQq6
        sKcZeL957zEvbNs8Pm22zQ5aJkbvDh6N4CCkiyFNtHLaBQTljoME6yLsCYyT
        utckL1vWWgAwGDtFoXkfqYskF7MTh7paVqvy4l8o+XIoyroolieBwOGmXl3d
        1olg/HfbO+iPtovaWRB9yiIBB7LkcdCfFCAqgs4Zk+QRYyRvc1eFYafNONAD
        prOwmI7/2D+j17gfkhPGyYzSK9ATSOq1JQlvOqpJyOm5Ts6ZwfuP9Sx8JxUc
        cuJpqDwHuj4Hqn4g9voF6VGoEg==

    [Y] First user-visible description
    [Y] First user-visible description
    [N] First user-visible description
        Detailed description of what went wrong.
            :: END REQUIREMENTS ::

Requirements Language
---------------------

The requirements checker can currently check only a very limited set
of conditions: that a hash of a given declaration matches an expected
value, that an identifier is defined by a top-level application of a
desired function, and that a function is defined using a limited form
of pattern matching.  This is by no means intended to be the final
constraint language; rather, it was the set of requirements needed for
a specific test case, and was therefore implemented first (in a hacky
way).  The language is strongly subject to change in the future.
