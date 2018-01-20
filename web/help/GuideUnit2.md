Defining Functions
==================

### Functions ###

Functions have their own types. Here are some examples:

* `light` is a function that needs a color and makes another color.  It
  has the type `Color -> Color`.
* `circle` is a function that needs a number (the radius), and makes a
  picture (a circle with that radius).  It has the type
  `Number -> Picture`.
* `rectangle` is a function that needs two numbers, and makes a picture.
  It has the type `(Number, Number) -> Picture`.
* `translated` is a function that needs a picture and two numbers (the x
  and y distances), and makes a new picture.  It has the type
  `(Picture, Number, Number) -> Picture`.

In general, function types have arrows (`->`).  On the left of the arrow
is the type of things that the function needs.  On the right side of the
arrow is the type of things that the function makes.

Writing Your Own Functions
------------------

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

Here's how you would define a house as a function that's waiting on a color
for the roof, and apply it to draw a house with a red roof.

    program = drawingOf(scene)
    scene   = house(red)

    house :: Color -> Picture
    house(roofColor) = colored(roof, roofColor) & solidRectangle(6, 7)

    roof :: Picture
    roof = translated(thickArc(45, 135, 6, 1), 0, -2)

Notice that before the equal sign, you give a name for the piece of missing
information, which is called the parameter.  When using the function, you need
to provide parentheses with specific values for those arguments.

Parameters to functions can be of any type.  The next example defines a
function with a picture as a parameter.

    program = drawingOf(ringOf(rectangle(1,1)))
    ringOf(p) = pictures([
        rotated(translated(p, 5, 0), a) | a <- [45, 90 .. 360] ])

The name `p` is given to the parameter to `ringOf`.  When `ringOf` is used in
the definition of `program`, it must be given a parameter, with a specific picture
to substitute for occurrences of the parameter `p`.

The idea of *substitution* is fundamental in how you define functions in
CodeWorld.  When you use a function, the body of the function is adapted by
finding all parameter names, and substituting the corresponding actual
parameters from where the function is used.

### Conditionals: Choosing what to do ###

All of the functions defined so far have basically the same form regardless of
their parameters.  Sometimes, you may want the definition to follow a different
form depending on the parameters.  In this case, you need a conditional.

The simplest kind of conditional uses `if`, `then`, and `else`.  Here's an
example:

    program  = drawingOf(thing(1) & thing(2))
    thing(n) = if n > 1 then rectangle(n, n) else circle(n)

This program displays one square, and one circle.  You can use `if`, `then`,
and `else` any place you can write an expression.  In your condition, you can
use any inequality (`<`, `>`, `<=`, or `>=`), or you can check whether two things
are equal using `==`.  Note the *two* equal signs: think of two equal signs as
a question (are these equal?), while the single equal sign used in a definition
is a statement (these things are equal!).

If you have more than two possibilities, you may want to use guards instead:

    program       = drawingOf(thing(1) & thing(2) & thing(3))
    thing(n)
      | n > 2     = rectangle(n, 2)
      | n > 1     = rectangle(n, n)
      | otherwise = circle(n)

This will draw a rectangle, a square, and a circle.  Each guard has a condition,
and if the condition matches, that choice is made for the definition.  Guards
are evaluated from the top down, so later guards only match if an earlier guard
hasn't matched already.

Finally, a special guard `otherwise` matches anything that reaches it.  Since
your program will crash if no guards match a function, it's usually a good idea
to include an `otherwise` guard just to make sure something matches no matter
what the parameters are.

### Pattern matching ###

So far, all of your functions have used variables to just name their parameters.
Sometimes, though, you want to dig inside of a parameters, and match its pieces.
You can do that, too.  Here's a really basic example:

    f :: [Number] -> Number
    f([a, b, c]) = a + b + c

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

    f :: [Number] -> Number
    f([]   ) = 42
    f([a]  ) = a + 1
    f(other) = sum(other)

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

    f(0) = 1
    f(1) = 2
    f(2) = 4
    f(n) = 10 * n

But this doesn't do what you think:

    f(pi   ) = 1
    f(other) = 2

You might think that function maps `pi` to the value `1` but everything else to
`2`.  Surprisingly, though, it actually gives a value of `1` for all inputs!
That's because `pi` is a variable, so the first equation is interpreted as
saying: match anything, and call it `pi`.  To write the function you meant, you
would need to write:

    f(x) | x == pi   = 1
         | otherwise = 2

Pattern matching, therefore, isn't the best way to match specific values,
because you can't use variables at all!  It is, however, a great way to write
functions that depend on the *structure* of a parameter.
