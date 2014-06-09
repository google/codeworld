Welcome to CodeWorld!
=====================

Using CodeWorld, you can create your own pictures, animations, and games.  This
guide will help you get started.

Tips for using this guide:

* If you feel the urge to play around with something and try new things, please
  do!  It won't do much good to just read.  You have to try things to learn.
* If you see a green box, you can click it to copy the code and try it out.
* If you need space on the screen, try closing the Browse pane.  You won't need
  it for this tutorial, and you can open it again any time.

Drawing With CodeWorld
======================

Definitions
-----------

A CodeWorld project is a bunch of *definitions*.  You write a definition to say what
something means.  For example, you might write:

    wheel = circle 10

This is a definition of "wheel": it says that "wheel" means a circle with a radius of
10 pixels.  *Radius* just means the distance from the center of the circle to the edge.
A pixel is a tiny dot on the computer screen, so 10 pixels is actually not very far!

In CodeWorld, you do absolutely everything by naming things.  We call the names (like
"wheel" in that example) *variables*.  So, earlier we defined a *variable* called
"wheel". You will define a lot of variables in CodeWorld.

### Defining main ###

Sooner or later, you need to say where to start.  You do that by defining a special
variable called *main*.  Every CodeWorld project needs *exactly* *one* definition for
*main*.  A complete program might look something like this:

    main  = pictureOf wheel
    wheel = circle 10

That's a complete project, so try it out!

Pictures
--------

You know how to draw a circle.  Now let's play around with some different basic
shapes:

* `circle 20`: Play around with the *radius* to get a feel for how different sizes
  look on the screen.
* `solidCircle 50`: Use `solidCircle` instead of `circle`, and your circle will be
  filled in.
* `rectangle 100 200`:  You can draw a rectangle by giving both a width and a height.
* `solidRectange 100 50`: Just like with circles, you can use `solidRectangle` to
  fill in the shape.
* `text "I Love Pandas!"`: You can write text (such as letters and words) to the
  screen by using `text`.  You need quotes around the words.

There are plenty more: `line`, `polygon`, `thickCircle`, `thickRectangle`, `arc`,
`sector`... the list goes on and on!  Don't worry; you will be able to play with
all of them.

### Combining Shapes ###

Pictures would be pretty boring if they could only have one shape.  Luckily, you
can combine more than one shape in the same picture using `&` (which means *and*).
For example:

    main   = pictureOf design
    design = solidRectangle 100 10
             & solidCircle 30
             & circle 50

Try that out, and see what it looks like!  See how the definition of design takes
more than one line?  That's okay: you can start a new line any time you want to.
However, *only* new definitions can start at the beginning of the line.  If you
start a new line inside of a definition, you have to *indent* it by leaving a few
spaces.  We often like to use those spaces to line things up, too.

When combining pictures, it helps if you remember to name things!  Another way of
writing the same program we just looked at is:

    main    = pictureOf design
    design  = slot & middle & outside
    slot    = solidRectangle 100 10
    middle  = solidCircle 30
    outside = circle 50

You will learn that it helps to think about more complicated pictures if you give
good names to the pieces.

### Colors ###

Pictures don't need to be black and white.  You can use `color` to change the color
of your pictures.  Here's a simple example:

    main     = pictureOf redWheel
    redWheel = color red wheel
    wheel    = solidCircle 100

You can also mix colors in the same picture:

    main   = pictureOf tree
    tree   = color green leaves & color brown trunk
    leaves = sector 0 180 100
    trunk  = solidRectangle 25 100

### Transformations ###

So far, all the pictures we've drawn have been at the middle of the screen.  That's
no fun.  But never fear, *transformations* are here!

Transformations are ways to change a picture.  There are three kinds of
transformations you can use in CodeWorld:

#### Translation: Moving Your Pictures ####

You can *translate* a picture to move it up, down, left, or right on the screen.
To use `translate`, you give it three things:

* A distance (how many pixels) to move the picture *left* or *right*.
  Negative numbers are left, and positive numbers are right.
* A distance (again, in pixels) to move the picture *up* or *down*.  Negative
  numbers are down, and positive numbers are up.
* A picture to move.

Ready for an example?

    main   = pictureOf forest
    forest =   translate (-125) ( 125) tree
             & translate (   0) (   0) tree
             & translate ( 125) (-125) tree
    tree   = color green leaves & color brown trunk
    leaves = sector 0 180 100
    trunk  = solidRectangle 25 100

A few comments are worth making:

1. Are you wondering why there are parentheses around the numbers?  You have
   to be careful with negative numbers, because the minus sign can sometimes
   look like subtraction instead.  If you write `translate -125`, it looks
   like `translate - 125`, and that's not what you meant!  The answer is to
   put parentheses around the negative number.
2. What does `translate 0 0` mean?  Well, it means don't move the picture at
   all!  We wrote the `translate` there just to make things line up nicely.

#### Rotation: Turning Your Pictures ####

You can *rotate* a picture to turn it, either clockwise or counter-clockwise.
To use `rotate`, you give it two things:

* A number of degrees to rotate the picture.  Negative numbers are clockwise,
  and positive numbers are counter-clockwise.
* A picture to rotate.

Here's an example:

    main    = pictureOf diamond
    diamond = rotate 45 square
    square  = solidRectangle 100 100

A diamond is just a square, turned so it's diagonal.

#### Scaling: Stretching Your Pictures ####

Finally, you can *scale* a picture to stretch it or flip it over, either
horizontally or vertically.  To use scale, you'll give:

* A factor by which to stretch the picture horizontally.  1 means leave it
  alone.  Numbers bigger than stretch it out, and numbers smaller than 1
  (like 0.5) squish it together to make it smaller.  Negative numbers flip
  the picture over, like looking at it in a mirror.
* A factor by which to stretch the picture vertically.  The meaning of
  numbers is the same.
* A picture to stretch.

Here's an example of `scale`:

    main = pictureOf oval
    oval = scale 2 0.5 base
    base = solidCircle 100

You should try to get a good feeling for the meaning of those scaling
factors.  Try changing the numbers in the example, and see if you can
guess what will happen before you press run.

### Putting It Together ###

TODO: Add a non-trivial example here.

Expressions
-----------

Now that you've spent some time trying out pictures, let's learn a few
more tricks you can use.  The part of a definition after the equal sign
is called an *expression*.  For example:

* `circle 100` is an expression.
* `color red (text "Help")` is also an expression.
* So is `rectangle 20 100 & circle 50`.
* `tree = leaves & trunk` is *not* an expression.  It's a *definition*
  instead.  But `leaves & trunk` is an expression.

Can you tell the difference?  Expressions describe something, but don't
give it a name.  But every definition has an expression inside, after
the equal sign.  So expressions are pretty important.

### Parentheses ###

Remember how we used `rotate`?  Here's a quick reminder:

    main    = pictureOf diamond
    diamond = rotate 45 square
    square  = rectangle 50 50

Nice!  However, naming everything like that can get tedious.  If you
have a simple shape, such as `rectangle 50 50`, you may not want
to bother giving it a name.  But then, how do you use it as part of
another definition?  The answer is parentheses!

To use one expression inside of another one, put it in parentheses.
Try it:

    main = pictureOf diamond
    diamond = rotate 45 (rectangle 50 50)

Or even:

    main = pictureOf (rotate 45 (rectangle 50 50))

Careful, though!  Parentheses are useful to avoid naming simple things,
but if you use parentheses inside of parentheses inside of parentheses,
pretty soon it's hard to tell what's going on!

### Numbers ###

Parentheses can be used for more than just pictures.  You can let the
computer work out math for you on numbers, too.  When you write math
expressions, you can use `+` and `-` the way you normally would.  To
multiply, use `*`.  To divide, use `/`.

Check out this program:

    main   = pictureOf design
    design = rotate   (1 * 180 / 3) (rectangle 100 10)
             & rotate (2 * 180 / 3) (rectangle 100 10)
             & rotate (3 * 180 / 3) (rectangle 100 10)

We could have written `0`, `60`, and `120` (the answers to those math
problems).  But this way, it's very clear what we are doing: dividing
180 degrees into thirds, and then rotating a rectangle by each amount.

Animations
==========

    main     = animationOf design
    design t = rotate (60*t) slot & middle & outside
    slot     = solidRectangle 100 10
    middle   = solidCircle 30
    outside  = circle 50

More Information
================

Do you want to know about everything you can use to build your CodeWorld project?
The collection of all variables and types you can use in CodeWorld is called the
*prelude*, and you can look through the whole thing!

> [Show Me the Prelude!][1]

[1]: ./doc/Prelude.html "Prelude API Documentation"
