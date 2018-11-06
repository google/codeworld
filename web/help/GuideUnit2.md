Defining Functions
==================

In the last part, you learned about creating drawings in CodeWorld by
describing pictures.  One of the most important tools was variables:
names for values that you can put together to build your final program.
A variable names a single value, such as a number, color, text, or
picture.

In this section, you will consider this question: what do you do when
you want to name a concept, but some of the details can change each time
you use it.  Flowers in a drawing might look similar, but have different
colors or sizes.  Some space aliens might have different numbers of
tentacles or eyes.  You could even name more general concepts: the idea
of a "ring" of pictures, arranged in a circle around the center, is the
same regardless of whether it's a ring of people, dogs, houses, or clouds.

In these situations, you need a function.  Functions capture ideas that
*depend* *on* some details that haven't been decided yet, such as the
color of a flower, or what to arrange in a ring.  They are incomplete,
still waiting for more information before they describe a specific
picture, number, color, etc.

You've used a lot of functions already: most of the words that your
computer already knows -- like `circle`, `rectangle`, `translated`, or
`dark` -- are examples of functions.  You didn't need to tell the computer
what those functions mean.  They were already *defined*.  You can also
define your own functions, and that's what you'll do in this part.

Function as a formula
---------------------

As you learned in the last section, a **function** is a relationship that
associates each possible input with a specific result.  The inputs to a
function are called *arguments*, and the type of input the function
expects is called its *domain*.  The type of *result* produced a function
is called its *range*.  Altogether, the function can be thought of as a
machine, of sorts, that turns arguments (input values) into results
(output values), like this.

**************************************************
*    input     .----------.     output
*      o------>| function +------>o
* (arguments)  '----------'    (result)
**************************************************

One way of describing a function is to make a table showing which
result is associated with each possible input.  For example, if `f` is
a function that associates any number with twice itself, then it has
this table.

Input: `x`    | Output: `f(x)`
:------------:|:-------------:
      2       |         4
      3       |         6
      4       |         8
      5       |        10
      6       |        12

Since there are infinitely many numbers, you could never make a complete
table!  But once you've recognized the pattern, you can imagine that the
table goes on forever.  This table in one important way to represent a
function.

As useful as function tables are, though, for understanding a function,
they don't tell you the pattern; they only leave you to recognize it
yourself.  Because computers aren't very good at guessing things,
function tables are not a good language for your next task: defining
your own function.  A different representation is better, and the one
you will use is a *formula*.

One way to understand functions as a formula is to start by writing a
sequence of equations, having the same information as the table above:

~~~~~
f(2) = 4
f(3) = 6
f(4) = 8
f(5) = 10
f(6) = 12
~~~~~

But again, it's impossible to write an equation for every possible input.
So instead, you will write one equation that captures the *pattern* in the
lines above:

~~~~~
f(x) = x * 2
~~~~~

This is a formula that defines a function.  It captures the pattern in the
explicit equations above.  The way to read a function formula is to read
a silent "for any value of `x`..." at the beginning.  So this one says:
For any value of `x`, `f(x)` is `x * 2`.  The `x` can be any new name that
you like.  It just names some part of the equations that differs each
time.

Abstracting functions
---------------------

This technique of capturing the parts of expressions that differ and naming
them as arguments is very powerful.  Any time you have a repeating pattern
of some kind, you can just name the parts that differ with arguments, and
write down the entire pattern.  This doesn't just work for numbers.  It's
okay for other types such as colors and pictures, as well.

Suppose you had a drawing of these three signs:

~~~~~ . clickable
program = drawingOf(signs)
signs = translated(sign1, -7, 0) & sign2 & translated(sign3, 7, 0)

sign1 = colored(lettering("Go"), white) &
        colored(solidRectangle(5, 5), green)
sign2 = colored(lettering("Stop"), white) &
        colored(solidRectangle(5, 5), red)
sign3 = colored(lettering("Caution"), white) &
        colored(solidRectangle(5, 5), orange)
~~~~~

Noticing that the three signs usually follow the same pattern, you can
write one function.  The steps to doing this are:

1. Name the parts that differ.  In this case, that's the label on the sign,
   and the color of the sign.
2. Write one equation giving the formula for this function.
3. Change the definitions of the original variables to use the function
   instead of repeating the pattern.

The result looks like this.

~~~~~ . clickable
program = drawingOf(signs)
signs = translated(sign1, -7, 0) & sign2 & translated(sign3, 7, 0)

sign(label, color) = colored(lettering(label), white) &
                     colored(solidRectangle(5, 5), color)

sign1 = sign("Go", green)
sign2 = sign("Stop", red)
sign3 = sign("Caution", orange)
~~~~~

As you get more practice, you might skip over defining the repeated pattern
entirely, and just write a function to begin with.  Suppose you are
defining a house, and you already know that you'll want to be able to
easily change the color of the roof.  You can define the house from the
start with an argument for the roof color.

~~~~~ . clickable
program = drawingOf(scene)
scene   = house(red)
house(roofColor) = colored(roof, roofColor) & frame
roof = translated(thickArc(45, 135, 6, 1), 0, -2)
frame = solidRectangle(6, 7)
~~~~~

Even though you didn't start with three houses, the result is the same.
Before the equal sign, you give a name for the piece of missing
information, which is the argument.  (Again, there's a silent phrase:
"For any value of `roofColor`, ...")  When using the function, you need
to fill the function's parentheses with specific values for those
arguments.

Arguments to functions can be of any type.  You've seen functions with
numbers, colors, and text as arguments.  The argument can even be a
picture.  Here's an example of a function with a picture as an argument.

~~~~~ . clickable
program = drawingOf(ringOf(rectangle(1,1)))
ringOf(pic) = rotated(translated(pic, 5, 0),  60) &
              rotated(translated(pic, 5, 0), 120) &
              rotated(translated(pic, 5, 0), 180) &
              rotated(translated(pic, 5, 0), 240) &
              rotated(translated(pic, 5, 0), 300) &
              rotated(translated(pic, 5, 0), 360)
~~~~~

The name `p` is given to the parameter to `ringOf`.  When `ringOf` is used in
the definition of `program`, it must be given a parameter, with a specific picture
to substitute for occurrences of the parameter `p`.

Substitution
------------

The idea of **substitution** is fundamental to how functions work.  Substitution
just means taking two expressions that have the same value, and substituting
one for the other.  Substitution never changes the meaning of an expression,
but it can be used to rewrite an expression in simpler terms until you know
what it means.

Simple examples of substitution don't need any variables at all!  Consider these
two equations.

~~~~~
eye = circle(1) & pupil
pupil = solidCircle(1/3)
~~~~~

The second equation tells you that the two expressions `pupil` and
`solidCircle(1/3)` have the same value.  Therefore, in the first line, you
can *substitute* the second expression in place of the word `pupil`, to get:

~~~~~
eye = circle(1) & solidCircle(1/3)
~~~~~

You've done these kinds of substitutions already.  Functions give you a new
way of doing substitution.  When you have a formula that describes the pattern
of a function, you can substitute any possible value of the argument in place
of the placeholder variable of the pattern, to get an equation for a specific
use of that function.  Consider this more complicated eye:

~~~~~
eye(color) = solidCircle(1/3) &
             colored(solidCircle(1/2), color) &
             circle(1)
~~~~~

This is a general pattern.  (Remember the implied "for any value of `color`..."
at the beginning.)  To decide which specific picture is meant by `eye(blue)`,
substitute the specific value you are interested in where ever the placeholder
appears.

~~~~~
eye(blue) = solidCircle(1/3) &
            colored(solidCircle(1/2), blue) &
            circle(1)
~~~~~

!!! Warning: Don't type the last block of code.
    This is one of many specific equations that are all implied by the overall
    *pattern* written earlier.  You do not need to tell the computer about eyes
    of each specific color.

Now this equation tells you that you can replace the expression `eye(blue)` with
the longer expression on the right-hand side.

Substitution is in some ways the opposite of abstraction.  Abstracting a function
means pulling out the parts that change you can write a more general pattern.
Substitution lets you start from a general pattern, and get an equation for any
specific choice of values.

Abstracting Expressions
=======================

This section to be written.

Equivalent expressions
----------------------

This section to be written.

### Identity ###

This section to be written.

### Associating ###

This section to be written.

### Distributing ###

This section to be written.

### Commuting ###

This section to be written.

## Splitting and combining ###

This section to be written.

Scope
=====

When you defined variables in the previous section, those variables could be
used anywhere in the code.  The variables that stand for arguments in functions
are different.  They only make sense inside of the definition of that function.
Remember, they stand for whatever expression is used for that argument when that
specific function is *used*.  There are words for these ideas.

* A **global variable** is a variable that can be used anywhere in your code.
* A **local variable** is a variable that can only be used in part of the code.

The *scope* of a variable is the part of code where it makes sense to use that
variable.  This sheds a new light on everyone's favorite error message:
"Variable not in scope"!  The message really means that you are using a name
that might be defined somewhere, but it doesn't make sense *here*.

Defining local variables
------------------------

You can actually add your own variables in *local scope*, as well.  Even if the
variable doesn't represent a function argument, you might want to limit how much
of your code sees that variable name just because it makes it easier to name
things!  For example, suppose your drawing includes a cat and a mouse.  You
might define variables called `head`, `tail`, and `body`, while you are working
on the cat.  When you start on the mouse, these names are already used for the
cat, and you have to pick something else.  (You could go back and rename them to
`catHead`, `catTail`, and `catBody`, but what a pain!)

One solution is to turn these into *local* variables, so the names don't cause
problems.  To do this, you can use the `where` clause.  This goes at the end of
the definition, and lets you define local variables that are only used inside of
that definition.  It looks like this:

~~~~~ . clickable
program = drawingOf(mouse)
mouse = head & body & tail
  where head = translated(solidCircle(1), 2, 2) &
               translated(solidCircle(1/4), 3, 2)
        body = solidCircle(2)
        tail = translated(solidRectangle(2, 1/4), -2, -1)
~~~~~

The formatting matters here!  The word `where` must be indented.  It cannot
start at the beginning of a new line, because it is still part of the definition
of `mouse`.  The three variables after `where` - `head`, `body`, and `tail`,
need to be indented as well, but they *also* need to line up in the same column
with each other.  If one of those definitions wraps to the next line (as `head`
does), it needs to be indented even *further* than the point where these
definitions begin.

Another nice thing about `where` is that because the definitions are local, you
*can* use the function's arguments in the `where` clause.  That makes it easier
to define parts of your pictures as simple old variables, even when they depend
on arguments.

Plumbing
--------

This section remains to be written.

Calculating with Arguments
==========================

This section remains to be written.

Tracking arithmetic
-------------------

This section remains to be written.

Intermediate results
--------------------

This section remains to be written.

Conditional Functions
=====================

All of the functions defined so far have basically the same form regardless of
their parameters.  Sometimes, you may want the definition to follow a different
form depending on the parameters.  In this case, you need a conditional
function.

To write a conditional function, instead of one equal sign and a right-hand
side, you'll right several of these, preceded by *guards*.  A guard gives a
statement that must be true for this right-hand side of the equation to apply.

~~~~~ . clickable
program       = drawingOf(thing(1) & thing(2) & thing(3))
thing(n)
  | n > 2     = rectangle(n, 2)
  | n > 1     = rectangle(n, n)
  | otherwise = circle(n)
~~~~~

This will draw a rectangle, a square, and a circle.  When each guard matches,
that choice is made for the definition.  Guards are evaluated from the top down,
so later guards only match if an earlier guard hasn't matched already.

In the guards, you can use any inequality (`<`, `>`, `<=`, or `>=`), or you can
check whether two things are equal using `==`.  Note the *two* equal signs:
think of two equal signs as a question ("Are these values equal?"), while the
single equal sign used in a definition is a statement ("These values are equal!").

Finally, a special guard `otherwise` matches anything that reaches it.  Since
your program will crash if no guards match a function, it's usually a good idea
to include an `otherwise` guard just to make sure something matches no matter
what the parameters are.

Pattern matching
----------------

So far, all of your functions have used variables to just name their parameters.
Sometimes, though, you want to dig inside of a parameters, and match its pieces.
You can do that, too.  Here's a really basic example:

~~~~~
f :: [Number] -> Number
f([a, b, c]) = a + b + c
~~~~~

The function `f` expects *one* parameter, which is a list.  But it then breaks
apart that list, and adds up three numbers that it finds inside.  If you ask
for `f([1, 2, 3])`, the `1` will be batched to `a`, the `2` to `b`, and the `3`
to `c`, for an answer of `6`.

But what if there aren't three elements in the list?  What if there are only
two?  Or four?  The answer, is just like when you have guards: if nothing
matches, the program will crash!  So `f([1])` is undefined.  So is
`f([1, 2, 3, 4])`.  The equation you've written *only* provides a value for `f`
when its parameter is a list of length exactly `3`.

The way you handle more cases is by writing multiple equations.  For example,
you might write:

~~~~~
f :: [Number] -> Number
f([])    = 42
f([a])   = a + 1
f(other) = sum(other)
~~~~~

This function is defined for more lists.  The first equation contains a pattern
that just matches the empty list.  The second matches a list of length one.  And
the final pattern matches anything else, by just using one variable name for the
entire list.

(As an aside, is `f` defined for all lists?  Surprisingly, no!  It's true that
all lists will match one of those equations.  But the `sum` function is *itself*
undefined for lists that are infinitely long.  So `f` will be, as well.)

To better understand pattern matching, it's important to know the difference
between a variable and a value.  On the right side of an equal sign, you can
always use a variable as a name for its value.  But on the left side of an equal
sign, a variable is just a parameter that will match *anything*, but a value
will only match itself.  So this does exactly what you might guess:

~~~~~
f(0) = 1
f(1) = 2
f(2) = 4
f(n) = 10 * n
~~~~~

But this doesn't do what you think:

~~~~~
f(pi)    = 1
f(other) = 2
~~~~~

You might think that function maps `pi` to the value `1` but everything else to
`2`.  Surprisingly, though, it actually gives a value of `1` for all inputs!
That's because `pi` is a variable, so the first equation is interpreted as
saying: match anything, and call it `pi`.  To write the function you meant, you
would need to write:

~~~~~
f(x) | x == pi   = 1
     | otherwise = 2
~~~~~

Pattern matching, therefore, isn't the best way to match specific values,
because you can't use variables at all!  It is, however, a great way to write
functions that depend on the *structure* of a parameter.

Design
======

This section to be written.

Principles of Design
--------------------

This section to be written.

### Contrast ###

This section to be written.

### Repetition ###

This section to be written.

### Alignment ###

This section to be written.

### Repetition ###

This section to be written.

Refactoring
-----------

This section to be written.
