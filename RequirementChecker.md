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

Here are the current three checks implemented.

- `matchesExpected(var, 999999)`

  Checks that the definition of `var`, once source locations are
  cleared, hashes to the given value (module 1 million).  This is
  used to verify that the student hasn't modified code they
  weren't supposed to change.

  The usual process for using this is to first write a failing
  check with some arbitrarily chosen hash.  The failure message
  will include the correct hash, so you can update the check.

- `hasSimpleParams(var)`

  Checks that `var` is defined as a function, all of whose arguments
  are plain variables.  Any use of more complex pattern matching will
  cause this requirements to fail.  It will also fail if `var` is not
  defined.

- `definedByFunction(var, func)`

  Checks that `var` is defined directly to be `func` applied to some
  arguments.  This example was implemented directly because it was the
  main point of the test class where we first tried out this feature.

The existing language should not be interpreted as any indicator of
future syntax.  In particular, it seems likely we will settle on a more
standard format in the future, rather than a custom parser.  Something
like YAML or HCL seems best.

### Desirable use cases

The following use cases have been proposed, but are not yet implemented.

- Checking specific syntax against a pattern.  The idea is that you should
  be able to say:
  
      decl_matches:
      
        foo __var_x __var_y = __var_x + __var_y^2 - sqrt __any

  The pattern will be parsed as a declaration, and then a search will
  happen for a matching declaration anywhere in the module.  Anything
  except `__blah` style names will be matched immediately, but these
  special names will have special behavior (above: `__any` matches
  anything, and `__var_x` or `__var_y` match any variable, but all
  matches of each must be the same.  Other special behavior might
  include allowing something to be repeated (so a function definition
  could have an arbitrary number of arguments or guards), etc.

- Style constraints.  e.g., all top-level definitions must have type
  declarations.  Or all lines must be 80 characters or less.

- No warnings.  Alternatively, no warnings of specific types.

- Count requirements.  There must be at least 10 defined variables.  Or
  three polygons.  Perhaps this could be accomplished with a cardinality
  constraint on the matching form above.

- Forbidden imported symbols or modules.  By whitelist or blacklist.
  Exceptions should be allowed for specific definitions (usually
  built-in).  For instance, you may want students to only use a given
  variable called `ellipse`, and not use `circle` on its own outside
  of that.

Open questions
==============

1. Can we support runtime testing?

One appealing idea is to also add runtime testing to the requirements
language.  This is much harder to do.

I foresee implementing this by using a GHC plugin to inject the testing
code at the beginning of `main`.  The blocker for this is that it
requires a later GHC, than CodeWorld currently uses.  At the same time,
we'd move requirements reporting from a diagnostic output at
compile-time to a runtime action.  Requirements that are checked at
compile time would be hard-coded into the result, but dynamic
requirements (checked at runtime) would be evaluated on each execution.

2. Can requirements be embedded into a URL, rather than comments in
   the code?

The main motivation here is that students are likely to just delete
the requirements block in their code, either intentionally or
accidentally.  Embedding the requirements into a URL would make them
more durable.  One idea is to make the URL just another source file,
which would be parsed for REQUIRES and XREQUIRES comments, and those
would be added to the requirements for the current file.

(How this interacts with the GHC plugin implementation is also
interesting.  I suspect we'd need to then embed the requirements
into plugin options.)
