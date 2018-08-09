Writing Your Own Functions
==================

You already know how to define variables, so that you can refer to a
shape by a simple name, and use it several times.  But sometimes, you don't
want *exactly* the same thing several times.  You want things with one or
two differences.  Maybe you want three faces, but with different sized eyes.
Or maybe you want three houses, with different colored roofs.  In these cases,
you want to write a new function.

A function is like a variable, but it is incomplete.  It is waiting for more
information, which needs to be provided when it is used.  Think of some of
the functions that we've already used.  `circle` is an incomplete shape: it
needs a radius.  `rectangle` needs a width and a height.  Similarly, you
can write your own functions that need their own parameters.

Function Definitions
--------------------

Here's how you would define a house as a function that's waiting on a color
for the roof, and apply it to draw a house with a red roof.

~~~~~ . clickable
program = drawingOf(scene)
scene   = house(red)

house :: Color -> Picture
house(roofColor) = colored(roof, roofColor) & solidRectangle(6, 7)

roof :: Picture
roof = translated(thickArc(45, 135, 6, 1), 0, -2)
~~~~~

Notice that before the equal sign, you give a name for the piece of missing
information, which is called the parameter.  When using the function, you need
to provide parentheses with specific values for those arguments.

Parameters to functions can be of any type.  The next example defines a
function with a picture as a parameter.

~~~~~ . clickable
program = drawingOf(ringOf(rectangle(1,1)))
ringOf(p) = rotated(translated(p, 5, 0),  60) &
            rotated(translated(p, 5, 0), 120) &
            rotated(translated(p, 5, 0), 180) &
            rotated(translated(p, 5, 0), 240) &
            rotated(translated(p, 5, 0), 300) &
            rotated(translated(p, 5, 0), 360)
~~~~~

The name `p` is given to the parameter to `ringOf`.  When `ringOf` is used in
the definition of `program`, it must be given a parameter, with a specific picture
to substitute for occurrences of the parameter `p`.

The idea of *substitution* is fundamental in how you define functions in
CodeWorld.  When you use a function, the body of the function is adapted by
finding all parameter names, and substituting the corresponding actual
parameters from where the function is used.

Scope
-----

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

### The `where` clause ###

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

Conditional Functions and Guards
================================

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
