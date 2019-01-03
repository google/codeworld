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
`:: REQUIREMENTS ::` block, which is formatted and presented to the
user by the environment.  Finally, the most complex piece is the
requirement specification language, described at the end.

Requirement Comments
--------------------

There are two ways to embed requirements into a source file: REQUIRES
comments, or XREQUIRES comments.  You will typically start with the
first during development, and then optionally switch to the second
when you're done.

A REQUIRES comment looks like this:

    {-
        REQUIRES

        Description: Some user-readable description.
        Rules:
        - << YAML-format description of 1st rule to check >>
        - << YAML-format description of 2nd rule to check >>
        - << YAML-format description of 3rd rule to check >>
    -}

`Description` is a user-readable arbitrary string, which is used in
the output to provide a section header summarizing this requirement.
The `Rules` are YAML machine-readable descriptions of the formal
rules that are verified.  Details on the rules are below. A program
can contain multiple REQUIRES blocks, one for each top-level
requirement.  Each requirement can then have any number of rules that
must pass before that requirement is satisfied.

An XREQUIRES comment is an obfuscated variation on REQUIRES.  Instead
of a single requirement, it contains a compressed and base64-encoded
list of requirements.  (It is possible to include multiple XREQUIRES
blocks, but it's not necessary.)  It looks like this:

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
at the comment.  It is merely obfuscation, not encryption.  Sometimes
it is valuable to help honest learners avoid peeking at the answers
by accident.  One writes an XREQUIRES comment by first writing
REQUIRES comments, and then replacing them with the obfuscated form
obtained from compiler output when running that program.  It is
suggested that you keep a link to the code with unobfuscated
requirements, to make it easier to update your requirements later.

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
    [Y] Second user-visible description
    [N] Third user-visible description
        Detailed description of what went wrong.
                      :: END REQUIREMENTS ::

The requirements block includes everything from `:: REQUIREMENTS ::`
to the matching `:: END REQUIREMENTS ::`.

The block begins with an obfuscated version of the requirements,
which can be used to replace the plain-text requirements (REQUIRES)
with the obfuscated version (XREQUIRES) as discussed above.  After this,
there are top-level lines beginning with one of `[Y] `, `[N] `, or
`[?] `.  These indicate, for each requirement, if the requirement is
satisfied, not satisfied, or if there was a problem checking the
requirement.  If the latter two cases, there are further lines
explaining what went wrong.

Requirements Language
---------------------

The requirements checker can currently check only a very limited set
of conditions. Here are the current checks implemented.

- `containsMatch`

  Example:

      containsMatch:
        template: |
          __func $any = $any
        cardinality:
          atLeast: 4
      explanation: The code defines fewer than 4 functions.

  Checks that the code contains a declaration matching the one given.
  The template is a standard Haskell declaration, except that it can
  contain certain wildcards.  Any identifier beginning with two
  underscores, like `__x`, is a placeholder that matches any chosen
  name.  Additionally, there's some additional extra syntax:

  - `$any` matches any pattern or expression at all.
  - `$var` matches any variable name.
  - `$con` matches any constructor name.
  - `$lit` matches any literal expression.
  - `$num`, `$char`, or `$str` match specific types of literals.
  - `$(tupleOf [| some template |])` matches any tuple whose elements
    each match the template in Oxford brackets (that is, between `[|`
    and `|]`).
  - `$(contains [| some template |])` matches any pattern or
    expressions with a subexpression that matches the template in
    Oxford brackets (that is, between `[|` and `|]`).
  - `$(allOf [ [| first template |], [| second template |] ])` matches
    any expression that matches all of the child templates.  Other
    logical combinators include `anyOf` and `noneOf`.

  This is the workhorse of structural rules.  It's fairly powerful,
  but comes with some downsides.  It is complex to use correctly, and
  gives poor descriptions to the user when it fails.  A common pattern
  is to use it with custom explanations ina  progressive sequence, like
  this.

      - all:
        - containsMatch:
            template: |
              f $any = $any
          explanation: The function f is not defined.

        - containsMatch:
            template: |
              f $var = $any
          explanation: f should use variables as placeholder for its arguments.

        - containsMatch:
            template: |
              f $var = $(contains [| translated |])
          explanation: f should use translation in its definition.

  Note the use of `all` (discussed below) which tries each rule in
  turn and stops on the first failure.

- `matchesExpected`

  Example:

      matchesExpected:
        variable: var
        hash: 999999

  Checks that the definition of `var`, once source locations are
  cleared, hashes to the given value (modulo 1 million).  This is
  used to verify that the student hasn't modified code they
  weren't supposed to change.

  The usual process for using this rule is to first write a
  failing check with some arbitrarily chosen hash.  The failure
  message will include the correct hash, so you can update the
  check.

- `notDefined`

  Example:

      notDefined: var

  Checks that there is no definition for a variable named `var`.  If
  there is, it fails.

- `usesAllParams`

  Example:

      usesAllParams: func

  Checks that `func` makes use of all of its named parameters.  If
  any parameters are not used in an equation, it fails.

- `notUsed`

  Example:

      notUsed: var

  Checks that there are no references to `var` in the module.

This is by no means intended to be the full constraint language;
rather, it is a small set of a few requirements that can be used for
testing.  The language is strongly subject to change in the future.

### Desirable use cases

The following use cases have been proposed, but are not yet implemented.

- Capturing .  The idea is that you should
  be able to say:
  
      declMatches: |

        foo __x __y = __x + __y^2 - sqrt $any

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

3. Can the implementation be moved to a source plugin?

This would be ideal, because it would open the door to more powerful
checks, such as those involving type unification, or other static
analysis already implemented in GHC.  For example,
http://hackage.haskell.org/package/inspection-testing shows some
very powerful uses of GHC via plugins to prove things about code.
