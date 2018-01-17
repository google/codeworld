Unit 1: Drawing With CodeWorld
======================

Definitions
-----------

Writing code in CodeWorld is like writing a dictionary or glossary.  Your
project is a bunch of *definitions*.  You write a definition to say what
something means.  For example, you might write:

    wheel = circle(2)

This is a definition of "wheel": it says that "wheel" means a circle with a radius of
2.  *Radius* just means the distance from the center of the circle to the edge.

In CodeWorld, you do absolutely everything by naming things.  We call the names (like
"wheel") *variables*.  So in this example, we defined a *variable* called
"wheel". You will define a lot of variables in CodeWorld.

### Defining program ###

Sooner or later, you need to say what program you want.  You do that by defining a
special variable called *program*.  Every CodeWorld project needs *exactly* *one*
definition for the *program* variable.  Finished code might look something like
this:

    program = drawingOf(wheel)
    wheel   = circle(2)

That's a complete project, so try it out!

Pictures
--------

You know how to draw a circle.  Now let's play around with some different basic
shapes:

* `circle(8)`: Play around with the *radius* to get a feel for how different sizes
  look on the screen.
* `circle(0.5)`: You can even use fractions or decimals for your radius.
* `solidCircle(5)`: Use `solidCircle` instead of `circle`, and your circle will be
  filled in.
* `rectangle(4,8)`:  You can draw a rectangle by giving both a width and a height.
* `rectangle(4,4)`:  A square is just a rectangle, where the width is the same as
  the height.
* `solidRectangle(8,4)`: Just like with circles, you can use `solidRectangle` to
  fill in the shape.
* `text("I Love Pandas!")`: You can write text (such as letters and words) to the
  screen by using `text`.  You need quotes around the words.

There are plenty more: `path`, which draws a line; `polygon`, which draws a polygon;
`thickCircle`, which draws a circle with a thicker line; `thickRectangle`, which is
the same as `thickCircle` but for rectangles; `arc`, which draws an arc; `sector`, 
which draws a filled in portion of a circle... the list goes on and on!  Don't worry; you will be able to play with
all of them.

### Combining Shapes ###

Pictures would be pretty boring if they could only have one shape.  Luckily, you
can combine more than one shape in the same picture using `&` (which means *and*).
For example:

    program = drawingOf(design)
    design  = solidRectangle(4, 0.4)
              & solidCircle(1.2)
              & circle(2)

Try that out, and see what it looks like!  See how the definition of design takes
more than one line?  That's okay: you can start a new line any time you want to.
However, *only* new definitions can start at the beginning of the line.  If you
start a new line inside of a definition, you have to *indent* it by leaving a few
spaces.  We often like to use those spaces to line things up, too.

When combining pictures, it helps if you remember to name things!  Another way of
describing the same program we just looked at is:

    program = drawingOf(design)
    design  = slot & middle & outside
    slot    = solidRectangle(4, 0.4)
    middle  = solidCircle(1.2)
    outside = circle(2)

You will learn that it helps to think about more complicated pictures if you give
good names to the pieces.

### Colors ###

Pictures don't need to be black and white.  You can use `colored` to change the color
of your pictures.  Here's a simple example:

    program  = drawingOf(redWheel)
    redWheel = colored(wheel, red)
    wheel    = solidCircle(4)

You can also mix colors in the same picture:

    program = drawingOf(tree)
    tree    = colored(leaves, green) & colored(trunk, brown)
    leaves  = sector(0, 180, 4)
    trunk   = solidRectangle(1, 4)

You can also modify the colors!  Here are a few ways to do that:

* `dark(red)`, means `red`, except a little darker.
* `light(green)` means `green`, but a little lighter.  So `light` is the opposite
  of `dark`.
* `translucent(blue)` means blue, but see-through.  The word "translucent" means
  partially transparent or see-through.

Let's try some example code:

    program = drawingOf(overlap)
    overlap = colored(square,  translucent(blue))
            & colored(disk, translucent(green))
    square  = solidRectangle(5, 5)
    disk    = solidCircle(3)

### Transformations ###

So far, all the pictures we've drawn have been at the middle of the screen.  That's
no fun.  But never fear, *transformations* are here!

Transformations are ways to change a picture.  There are three kinds of
transformations you can use in CodeWorld:

#### Translation: Moving Your Pictures ####

You can *translate* a picture to move it up, down, left, or right on the screen.
To use `translated`, you give it three things:

* A picture to move.
* A distance to move the picture *left* or *right*. Negative numbers
  are left, and positive numbers are right.
* A distance to move the picture *up* or *down*.  Negative numbers are down,
  and positive numbers are up.

Ready for an example?

    program = drawingOf(forest)
    forest  = translated(tree, -5, 5)
            & translated(tree,  0, 0)
            & translated(tree,  5,-5)
    tree    = colored(leaves, green) & colored(trunk, brown)
    leaves  = sector(0, 180, 4)
    trunk   = solidRectangle(1, 4)

What does `translated(..., 0, 0)` mean?  Well, it means don't move the picture at
all!  We wrote the `translated` there just to make things line up nicely.

#### Rotation: Turning Your Pictures ####

You can *rotate* a picture to turn it, either clockwise or counter-clockwise.
To use `rotated`, you give it two things:

* A picture to rotate.
* A number of degrees to rotate the picture.  Negative numbers are clockwise,
  and positive numbers are counter-clockwise.

Here's an example:

    program = drawingOf(diamond)
    diamond = rotated(square, 45)
    square  = solidRectangle(4, 4)

A diamond is just a square, turned so it's diagonal.

#### Scaling: Stretching Your Pictures ####

Finally, you can *scale* a picture to stretch it or flip it over, either
horizontally or vertically.  To use `scaled`, you'll give:

* A picture to stretch.
* A factor by which to stretch the picture horizontally.  1 means leave it
  alone.  Numbers bigger than 1 stretch it out, and numbers smaller than 1
  (like 0.5) squish it together to make it smaller.  Negative numbers flip
  the picture over, like looking at it in a mirror.
* A factor by which to stretch the picture vertically.  The meaning of
  numbers is the same.

Here's an example of `scaled`:

    program = drawingOf(oval)
    oval    = scaled(base, 2, 0.5)
    base    = solidCircle(4)

You should try to get a good feeling for the meaning of those scaling
factors.  Try changing the numbers in the example, and see if you can
guess what will happen before you press run.

Expressions
-----------

Now that you've spent some time trying out pictures, let's learn a few
more tricks you can use.  Anything you can write after the equal sign
is called an *expression*.  For example:

* `circle(4)` is an expression.
* `colored(text("Help"), red)` is also an expression.
* `rectangle(1, 4) & circle(2)` is an expression.
* `leaves & trunk` is an expression.

However, `tree = leaves & trunk` is *not* an expression.  It's a
definition.  Can you tell the difference?  Expressions describe
something, but don't give it a name.  But every definition has an
expression inside, after the equal sign.  So expressions are pretty
important.

### Functions ###

One special kind of expression that you use a lot in CodeWorld is
applying a *function*.  A function is like a variable that still needs
more information.  You've already used a lot of functions:

* `rectangle` is a function.  It needs a width and a height, and makes a
  picture.
* `light` is a function.  It needs a color, and makes another color that's
  about the same, but lighter.
* `drawingOf` is a function.  It needs a picture, and makes a program to
  draw that picture.
* `scaled` is a function.  It needs a picture and two scaling factors, and
  makes a modified picture.

As you've already seen, to apply a function, you can write the function
name, then the extra information it needs (these are called *parameters*)
in parentheses after it, with commas between them.

### Nesting ###

Remember how we used `rotated`?  Here's a quick reminder:

    program = drawingOf(diamond)
    diamond = rotated(square, 45)
    square  = rectangle(2, 2)

Nice!  However, naming everything like that can get tedious.  If you
have a simple shape, such as `rectangle(2, 2)`, you may not want
to bother giving it a name.  You can just describe the shape right where
the name would go.

Try it:

    program = drawingOf(diamond)
    diamond = rotated(rectangle(2, 2), 45)

Or even:

    program = drawingOf(rotated(rectangle(2, 2), 45))

Careful, though!  You can avoid avoid naming simple things, but if you
nest too much, you get parentheses inside of parentheses inside of
parentheses,  and pretty soon it's hard to tell what's going on!

You can also nest other things besides pictures.  Remember that `dark`,
`light`, and `transparent` were functions that modify colors.  But since
`dark(red)` and `light(green)` are colors themselves, so you can use the
same functions on them!  Check out some of these colors:

* `dark(dark(green))`
* `translucent(light(blue))`

### Numbers ###

Nesting can be used for numbers, too.  You can let the computer work out
math for you on numbers, too.  When you write math expressions, you can
use `+` and `-` the way you normally would.  To multiply, use `*`.  To
divide, use `/`.

Check out this code:

    program = drawingOf(design)
    design  = rotated(rectangle(4, 0.2), 1 * 180 / 5)
            & rotated(rectangle(4, 0.2), 2 * 180 / 5)
            & rotated(rectangle(4, 0.2), 3 * 180 / 5)
            & rotated(rectangle(4, 0.2), 4 * 180 / 5)
            & rotated(rectangle(4, 0.2), 5 * 180 / 5)

We could have written `36`, '72', '108', '144', and `180` (the answers to
those math problems).  But this way, it's very clear what we are doing:
dividing 180 degrees into fifths, and then rotating a rectangle by each
amount.  And we don't have to worry about getting one of the answers
wrong!

Just like in math, you can use parentheses to group expressions, so
`3 * (6 - 2)` is `3 * 4`, which is `12`.
