Defining functions
==================

In the last part, you learned about creating drawings in CodeWorld by
describing pictures.  One of the most important tools was variables:
names for values that you can put together to build your final program.
A variable names a single value, such as a number, color, text, or
picture.  But sometimes you want to name a more general idea, and some
of the details can change each time you use it.  Flowers in a drawing
might look similar, but have different colors or sizes.  Some space
aliens might have different numbers of tentacles or eyes.  You could
even name more abstract concepts: the idea of a "ring" of pictures,
arranged in a circle, is the same whether it's a ring of people, dogs,
houses, or clouds.

In these situations, you need a function.  Functions capture ideas that
*depend* *on* some details that haven't been decided yet, such as the
color of a flower, or what to arrange in a ring.  They are incomplete,
still waiting for more information before they can describe a specific
picture, number, color, etc.

You've used a lot of functions already: most of the words that your
computer already knows -- like `circle`, `rectangle`, `translated`, or
`dark` -- are examples of functions.  You didn't need to tell the computer
what those functions mean.  They were already *defined* for you.  But
you can also define your own functions, and that's what you'll do in this
section.

Function as a formula
---------------------

As you learned in the last section, a **function** is a relationship that
associates each possible input with a specific result.  The inputs to a
function are called *arguments*, and the type of input the function
expects is called its *domain*.  The type of *result* produced a function
is called its *range*.  Altogether, the function can be thought of as a
kind of machine that turns arguments (input values) into results (output
values), like this.

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

You can write the same information in a form the computer understands
by writing equations, like this:

~~~~~
f :: Number -> Number
f(2) = 4
f(3) = 6
f(4) = 8
f(5) = 10
f(6) = 12
~~~~~

!!! Tip: The first line is a type signature.
    Type signatures are optional, but if you provide them, they describe
    the domain and range of the function.

Since there are infinitely many numbers, you could never make a complete
table or write equations for every possible argument!  Once you've
recognized the pattern, you can imagine that it goes on that way for any
number you put in.  Your computer, though, can't recognize patterns on
its own, so it needs a little more help.  Since it's impossible to write
an equation for every possible input, you can instead write one equation
that captures the *common pattern* in the equations above:

~~~~~
f(x) = x * 2
~~~~~

This is a formula representation of the function.  It captures the pattern
in the specific equations above.  The way to read a function formula is to
read a silent "for any value of `x`..." at the beginning.  So this one says:
For any value of `x`, `f(x)` is `x * 2`.  The `x` can be any new name that
you like.  It just names the parts of the equations that differ each time.

Generalizing patterns
---------------------

This technique of capturing the parts of expressions that differ and naming
them as arguments is very powerful.  You are working from *specific*
examples to a *general* pattern, a process called **generalization**.  Any
time you have a repeating pattern of some kind, you can just name the parts
that differ with arguments, and write down the entire pattern.  This
doesn't just work for numbers.  You can do the same for other types such as
colors and pictures, as well.

Suppose you had a drawing of these three signs:

~~~~~ . clickable
program = drawingOf(signs)
signs = translated(sign1, -7, 0) & sign2 & translated(sign3, 7, 0)

sign1 = colored(lettering("Go"), White) &
        colored(solidRectangle(5, 5), Green)
sign2 = colored(lettering("Stop"), White) &
        colored(solidRectangle(5, 5), Red)
sign3 = colored(lettering("Caution"), White) &
        colored(solidRectangle(5, 5), Orange)
~~~~~

If you notice that the three signs usually follow the same pattern, you can
write one function.  The steps to doing this are:

1. Name the parts that differ.  In this case, that's the label on the sign,
   and the color of the sign.
2. Write one equation giving the general formula for this function.
3. Change the definitions of the original variables to use the function
   instead of repeating the pattern.

The result looks like this.

~~~~~ . clickable
program = drawingOf(signs)
signs = translated(sign1, -7, 0) & sign2 & translated(sign3, 7, 0)

sign(label, color) = colored(lettering(label), White) &
                     colored(solidRectangle(5, 5), color)

sign1 = sign("Go", Green)
sign2 = sign("Stop", Red)
sign3 = sign("Caution", Orange)
~~~~~

As you get more practice, you might skip over defining the repeated pattern
entirely, and just write a function to begin with.  Suppose you are
defining a house, and you already know that you'll want to be able to
easily change the color of the roof.  You can define the house from the
start with an argument for the roof color:

~~~~~ . clickable
program = drawingOf(scene)
scene   = house(Red)
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

The name `pic` is given to the parameter to `ringOf`.  When `ringOf` is used in
the definition of `program`, it must be given a parameter, with a specific picture
to substitute for occurrences of the parameter `pic`.

!!! collapsible: What other function could you write to simplify this code?
    Any time you see the same code written many times, you should think of it
    as an opportunity for a function.  Here, there are many lines with the
    same nested rotation and translation.  This code can be rewritten to use
    a function instead.

    ~~~~~ . clickable
    program = drawingOf(ringOf(rectangle(1,1)))
    ringOf(pic) = part(pic,  60) & part(pic, 120) & part(pic, 180) &
                  part(pic, 240) & part(pic, 300) & part(pic, 360)
    part(pic, angle) = rotated(translated(pic, 5, 0), angle)
    ~~~~~

    You might find this program shorter and easier to read than the original.
    It's okay if you don't prefer it, though.  Just like writing in Human
    languages, there are different styles in code, and as you learn, you will
    develop your own voice and make your own decisions about how to
    communicate well.

The results of functions can also be different types.  For example, "pastel"
refers to a color that's soft and pale, such as a baby blue or lavender.
Compared to full colors, a pastel color is lighter, and more gray.  Your
computer doesn't know what a pastel color is... **yet**.  Let's teach it!

~~~~~
pastel(c) = light(dull(c))
neon(c) = dark(bright(c))
~~~~~

With these functions defined, you can use colors like `neon(Green)` or
`pastel(Yellow)`.  But it doesn't stop there: you can define functions with any
type of value that you like: pictures, numbers, colors, text... you name it.

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
at the beginning.)  To decide which specific picture is meant by `eye(Blue)`,
substitute the specific value you are interested in where ever the placeholder
appears.  One specific instance of this equation is this.

~~~~~
eye(Blue) = solidCircle(1/3) &
            colored(solidCircle(1/2), Blue) &
            circle(1)
~~~~~

!!! Warning: Don't type the last block of code.
    This is one of many specific equations that are all implied by the overall
    pattern written earlier.  You do not need to tell the computer about eyes
    of each specific color, because you've already told it the *pattern*.

This equation tells you that you can replace the expression `eye(Blue)` with
the longer expression on the right-hand side.

Substitution is in some ways the opposite of generalization.  Generalizing a
function means pulling out the specific parts that change, so you can write a
more general pattern.  Substitution lets you start from a general pattern, and
get an equation for any specific choice of values.

Unifying expressions
====================

So far, we've only written functions for examples where several definitions
had exactly the same form or shape, and the differences were in the choice of
specific values, like colors or numbers.  That's a good start, but functions
will become a lot more useful when you can be a little more flexible, by
rewriting expressions to have the same form, even if they didn't start out
that way.

Let's look at an example.  Consider these three definitions:

~~~~~
a = colored(solidCircle(1), Red)
b = colored(solidCircle(2), Blue)
c = solidCircle(3)
~~~~~

It's straightforward to generalize these into a function that can be used for
either `a` or `b`.  That function would be

~~~~~
f(radius, color) = colored(solidCircle(radius), color)

a = f(1, Red)
b = f(2, Blue)
~~~~~

But what about `c`?  At first glance, it doesn't seem to follow the same
pattern.  But wait a second.  The definition of `c` can be rewritten like
this:

~~~~~
c = colored(solidCircle(3), Black)
~~~~~

The `solidCircle` function already produces pictures of black circles, so the
extra `colored` function doesn't change anything at all.  But it *does* make
`c` follow the same pattern, so that it can be rewritten as

~~~~~
c = f(3, Black)
~~~~~

To use functions more effectively, then, it helps to know techniques for
rewriting expressions so that they look different, but have the same
value.  You can apply these techniques to write a wider variety of different
pictures

Equivalent expressions
----------------------

You already know some ways to write the same expression in different ways.
For example, you can write the number 6 as just `6`, or as `6 + 0`, or
`2 * 3`, or `6 * 1`, or even `3 * 3 - 3`.  When it comes to expressions that
describe pictures, you may be less familiar with how to easily move and
add and drop transformations and such.  In this section, you'll see more
some examples of different ways to rewrite expressions to have the same
values.

The examples here are mostly about pictures, but the ideas come up again
and again, for values of many types.

### Identities ###

It's useful to know when an operation doesn't change a value at all.  For
example, multiplying by 1 or adding 0 doesn't change a number.  There are
also operations on pictures that don't change them.  If `pic` is a picture
variable, then these expressions all have the same value regardless of which
specific picture it is.

~~~~~
pic
~~~~~

~~~~~
pic & blank
~~~~~

~~~~~
translated(pic, 0, 0)
~~~~~

~~~~~
rotated(pic, 0)
~~~~~

~~~~~
dilated(pic, 1)
~~~~~

~~~~~
scaled(pic, 1, 1)
~~~~~

An operation that doesn't change the value it's operating on is called an
**identity operation**.  Explore these examples to see how you can use
identity operations, such as overlaying a blank picture or rotating by 0
degrees, to rewrite expressions so they follow the same pattern.

!!! collapsible: Rewriting expressions with `blank`
    Remember that **`&`** is an operator for pictures, just like **`+`** is for
    numbers.  And just like adding zero doesn't change a number, `blank` is a
    picture that doesn't change anything it's combined with using `&`.  (The
    picture `blank` is completely transparent, so overlaying it in front of or
    behind another picture doesn't change what you see.)

    As an example of rewriting an expression using `blank` to generalize
    a function, consider this code.

    ~~~~~ . clickable
    program = drawingOf(closet)

    closet = translated(shirt1, -5, -5) & shirt2 & translated(shirt3, 5, 5)

    shirt1 = lettering("lol") & colored(shape, Blue)
    shirt2 = dilated(codeWorldLogo, 1/4) & colored(shape, Red)
    shirt3 = colored(shape, Yellow)

    shape = solidRectangle(6, 8) & thickArc(20, 160, 4, 2)
    ~~~~~

    The first two shirts combine some kind of picture as a design on the
    front with a shirt shape behind it.  The third, though, has no design.
    However, it can be rewritten as `blank & colored(shape, Yellow)`, and
    it now follows the same pattern, which can be generalized into a
    function.

    ~~~~~ . clickable
    program = drawingOf(closet)

    closet = translated(shirt1, -5, -5) & shirt2 & translated(shirt3, 5, 5)

    shirt1 = shirt(lettering("lol"), Blue)
    shirt2 = shirt(dilated(codeWorldLogo, 1/4), Red)
    shirt3 = shirt(blank, Yellow)

    shirt(design, color) = design & colored(shape, color)
    shape = solidRectangle(6, 8) & thickArc(20, 160, 4, 2)
    ~~~~~

!!! collapsible: Rewriting expressions with identity transformations.
    The geometric transformations -- `translated`, `rotated`, `dilated`,
    and `scaled` -- each have arguments where they don't change the
    picture at all.  For `translated` and `rotated`, which act like
    addition, those arguments are zero.  For `dilated` and `scaled`,
    which act like multiplication, those arguments are one.

    This means that if you are trying to generalize a collection of
    variables, but one of them is missing a transformation, you can
    add the missing transformation with zeros or ones to rewrite the
    expression in the same form as other examples.

    An example here is still to be written.

### Distributing transformations ###

(This section should be expanded.)

For numbers, the **distributive property** tells you that multiplying by
a sum, like `3 * (5 + 7)`, is the same as multiplying each part of the
sum, like `3 * 5 + 3 * 7`.  The operations we have on pictures are
different, but they have their own distributive properties.  Namely,
translating, rotating, dilating, or scaling an overlay of two pictures,
such as `a & b`, is the same as doing the same transformation to each
part of the overlay.

Here's what that means in equations.

~~~~~
translated(a & b, x, y) = translated(a, x, y) & translated(b, x, y)
~~~~~

~~~~~
rotated(a & b, angle) = rotated(a, angle) & rotated(b, angle)
~~~~~

~~~~~
dilated(a & b, k) = dilated(a, k) & dilated(b, k)
~~~~~

~~~~~
scaled(a & b, kx, ky) = scaled(a, kx, ky) & scaled(b, kx, ky)
~~~~~

In general, `colored` is the one transformation that cannot be distributed
in this way.  That is, `colored(a & b, color)` can sometimes be a different
picture from `colored(a, color) & colored(b, color)`.  The problem arises
when `a` and `b` overlap, and `color` has transparency.  In this case,
`colored(a & b, color)` applies the color consistently throughout `pic`,
including the areas of overlap.  But applying the color to each part and
then overlaying them creates a visible region of overlap.

In cases where `a` and `b` are known not to overlap, or where `color` is
known to be opaque, then `colored` can be distributed.

### Commuting transformations ###

(This section should be expanded.)

You've already seen that it can make a big difference which order
transformations are applied in, so in general you can't just change that
order to write expressions in the same patterns.  There are some particular
cases, though, where you can do precisely that.

The `colored` function **commutes** with any other transformation except
another use of `colored`.  That means it never matters whether you apply
`colored` inside or outside of `translated`, `rotated`, `dilated`, or
`scaled`.  Here's how we say that with equations.

~~~~~
colored(translated(pic, x, y), color) = translated(colored(pic, color), x, y)
~~~~~

~~~~~
colored(rotated(pic, angle), color)   = rotated(colored(pic, color), angle)
~~~~~

~~~~~
colored(dilated(pic, k), color)       = dilated(colored(pic, color), k)
~~~~~

~~~~~
colored(scaled(pic, kx, ky), color)   = scaled(colored(pic, color), kx, ky)
~~~~~

Another pair of transformations that commute with each other are `rotated`
and `dilated`.  You can rotate and then dilate, or dilate and then rotate,
and you'll get the same picture either way.

~~~~~
dilated(rotated(pic, angle), k) = rotated(dilated(pic, k), angle)
~~~~~

Be careful, though!  The same is not true of scaling.  Rotation commutes with
dilation because the dilation does the same thing to points in all *directions*,
but scaling acts differently in different directions, so rotating (which changes
direction) makes a difference.  In fact, most other transformations don't
commute directly.

Translation almost commutes with dilation or scaling: swapping them just
requires changing the distance of the transformation by multiplying (or dividing)
by the scaling factor.

~~~~~
dilated(translated(pic, x, y), k) =
    translated(dilated(pic, k), k * x, k * y)
~~~~~

~~~~~
scaled(translated(pic, x, y), kx, ky) =
    translated(scaled(pic, kx, ky), kx * x, ky * y)
~~~~~

### Splitting and combining ###

(This section should be expanded.)

One more way to rewrite expressions is to split a transformation into
two of the same transformation.  For example, translating by 5 units is
the same thing as translating by 3 units, and then further translating
that picture by 2 more.  Translation and rotation both work in this way:
if you combine two in a row, that's the same as adding the amounts.

~~~~~
translated(translated(pic, x1, y1), x2, y2) =
    translated(pic, x1 + x2, y1 + y2)
~~~~~

~~~~~
rotated(rotated(pic, angle1), angle2) =
    rotated(pic, angle1 + angle2)
~~~~~

Recall that dilation and scaling are more like multiplication.  (Think about
how you might use dilation to make a picture three *times* the size.)  To
combine two of these in a row, you multiply the amounts.

~~~~~
dilated(dilated(pic, k1), k2) = dilated(pic, k1 * k2)
~~~~~

~~~~~
scaled(scaled(pic, kx1, ky1), kx2, ky2) =
    scaled(pic, kx1 * kx2, ky1 * ky2)
~~~~~

### Other equivalences ###

(This section should be expanded.)

In addition to the categories above, there are a bunch of miscellaneous
equivalences that are worth seeing, since they let you rewrite things in
even more ways.

~~~~~
blank = lettering("")
blank = polygon([])
~~~~~

~~~~~
circle(r) = arc(0, 360, r)
~~~~~

~~~~~
rectangle(w, h) = polygon([
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)])
~~~~~

~~~~~
dilated(pic, k) = scaled(pic, k, k)
~~~~~

~~~~~
dilaled(pic, -1) = rotated(pic, 180)
~~~~~

~~~~~
rotated(pic, angle) = rotated(pic, angle + 360)
~~~~~

~~~~~
translated(polygon([(x1, y1), (x2, y2), (x3, y3)]), x, y) =
    polygon([(x1 + x, y1 + y), (x2 + x, y2 + y), (x3 + x, y3 + y)])
~~~~~

~~~~~
scaled(polygon([(x1, y1), (x2, y2), (x3, y3)]), kx, ky) =
    polygon([(x1 * kx, y1 * ky), (x2 * kx, y2 * ky), (x3 * kx, y3 * ky)])
~~~~~

~~~~~
rotated(polygon([(x1, y1), (x2, y2), (x3, y3)]), angle) =
    polygon([rotatedPoint((x1, y1), angle),
             rotatedPoint((x2, y2), angle),
             rotatedPoint((x3, y3), angle)])
~~~~~

There are functions you can use to translate, rotate, dilate, or scale
individual points, too.

~~~~~
translatedPoint :: (Point, Number, Number) -> Point
rotatedPoint :: (Point, Number) -> Point
dilatedPoint :: (Point, Number) -> Point
scaledPoint :: (Point, Number, Number) -> Point
~~~~~

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

(This section remains to be written.)

Calculating with arguments
==========================

(This section remains to be written.)

Tracking arithmetic
-------------------

(This section remains to be written.)

Intermediate results
--------------------

(This section remains to be written.)

Conditional functions
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

(This section remains to be written.)

Principles of design
--------------------

(This section remains to be written.)

### Contrast ###

(This section remains to be written.)

### Repetition ###

(This section remains to be written.)

### Alignment ###

(This section remains to be written.)

### Repetition ###

(This section remains to be written.)

Abstraction
-----------

(This section remains to be written.)

Refactoring
-----------

(This section should be expanded.)

Computer programmers use the word **refactoring** to describe changes to
their code that don't change the program, but just describe it differently.
The goal of refactoring is usually to communicate better to other humans
who read the code, or to reorganize the code so that it's easier to make
other changes.

The techniques you've learned in this part of the guide -- rewriting
expressions in equivalent ways, capturing repeated patterns, controlling
scope, building strong abstractions, and using design principles to
communicate more clearly in code -- are the basic tools of refactoring.
