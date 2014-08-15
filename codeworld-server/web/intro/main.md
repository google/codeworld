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

    wheel = circle(2)

This is a definition of "wheel": it says that "wheel" means a circle with a radius of
2.  *Radius* just means the distance from the center of the circle to the edge.

In CodeWorld, you do absolutely everything by naming things.  We call the names (like
"wheel" in that example) *variables*.  So, earlier we defined a *variable* called
"wheel". You will define a lot of variables in CodeWorld.

### Defining main ###

Sooner or later, you need to say where to start.  You do that by defining a special
variable called *main*.  Every CodeWorld project needs *exactly* *one* definition for
*main*.  A complete program might look something like this:

    main  = pictureOf(wheel)
    wheel = circle(2)

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

There are plenty more: `line`, `polygon`, `thickCircle`, `thickRectangle`, `arc`,
`sector`... the list goes on and on!  Don't worry; you will be able to play with
all of them.

### Combining Shapes ###

Pictures would be pretty boring if they could only have one shape.  Luckily, you
can combine more than one shape in the same picture using `&` (which means *and*).
For example:

    main   = pictureOf(design)
    design = solidRectangle(4, 0.4)
             & solidCircle(1.2)
             & circle(2)

Try that out, and see what it looks like!  See how the definition of design takes
more than one line?  That's okay: you can start a new line any time you want to.
However, *only* new definitions can start at the beginning of the line.  If you
start a new line inside of a definition, you have to *indent* it by leaving a few
spaces.  We often like to use those spaces to line things up, too.

When combining pictures, it helps if you remember to name things!  Another way of
writing the same program we just looked at is:

    main    = pictureOf(design)
    design  = slot & middle & outside
    slot    = solidRectangle(4, 0.4)
    middle  = solidCircle(1.2)
    outside = circle(2)

You will learn that it helps to think about more complicated pictures if you give
good names to the pieces.

### Colors ###

Pictures don't need to be black and white.  You can use `color` to change the color
of your pictures.  Here's a simple example:

    main     = pictureOf(redWheel)
    redWheel = color(wheel, red)
    wheel    = solidCircle(4)

You can also mix colors in the same picture:

    main   = pictureOf(tree)
    tree   = color(leaves, green) & color(trunk, brown)
    leaves = sector(0, 180, 4)
    trunk  = solidRectangle(1, 4)

You can also modify the colors!  Here are a few ways to do that:

* `dark(red)`, means `red`, except a little darker.
* `light(green)` means `green`, but a little lighter.  So `light` is the opposite
  of `dark`.
* `translucent(blue)` means blue, but see-through.  The word "translucent" means
  partially transparent or see-through.

Let's try an example program:

    main    = pictureOf(overlap)
    overlap = color(square,  translucent(blue))
            & color(disk, translucent(green))
    square  = solidRectangle(5, 5)
    disk    = solidCircle(3)

### Transformations ###

So far, all the pictures we've drawn have been at the middle of the screen.  That's
no fun.  But never fear, *transformations* are here!

Transformations are ways to change a picture.  There are three kinds of
transformations you can use in CodeWorld:

#### Translation: Moving Your Pictures ####

You can *translate* a picture to move it up, down, left, or right on the screen.
To use `translate`, you give it three things:

* A picture to move.
* A distance (how many pixels) to move the picture *left* or *right*.
  Negative numbers are left, and positive numbers are right.
* A distance (again, in pixels) to move the picture *up* or *down*.  Negative
  numbers are down, and positive numbers are up.

Ready for an example?

    main   = pictureOf(forest)
    forest =   translate(tree, -5, 5)
             & translate(tree,  0, 0)
             & translate(tree,  5,-5)
    tree   = color(leaves, green) & color(trunk, brown)
    leaves = sector(0, 180, 4)
    trunk  = solidRectangle(1, 4)

What does `translate(..., 0, 0)` mean?  Well, it means don't move the picture at
all!  We wrote the `translate` there just to make things line up nicely.

#### Rotation: Turning Your Pictures ####

You can *rotate* a picture to turn it, either clockwise or counter-clockwise.
To use `rotate`, you give it two things:

* A picture to rotate.
* A number of degrees to rotate the picture.  Negative numbers are clockwise,
  and positive numbers are counter-clockwise.

Here's an example:

    main    = pictureOf(diamond)
    diamond = rotate(square, 45)
    square  = solidRectangle(4, 4)

A diamond is just a square, turned so it's diagonal.

#### Scaling: Stretching Your Pictures ####

Finally, you can *scale* a picture to stretch it or flip it over, either
horizontally or vertically.  To use scale, you'll give:

* A picture to stretch.
* A factor by which to stretch the picture horizontally.  1 means leave it
  alone.  Numbers bigger than stretch it out, and numbers smaller than 1
  (like 0.5) squish it together to make it smaller.  Negative numbers flip
  the picture over, like looking at it in a mirror.
* A factor by which to stretch the picture vertically.  The meaning of
  numbers is the same.

Here's an example of `scale`:

    main = pictureOf(oval)
    oval = scale(base, 2, 0.5)
    base = solidCircle(4)

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

* `circle(4)` is an expression.
* `color(text("Help"), red)` is also an expression.
* So is `rectangle(1, 4) & circle(2)`.
* `tree = leaves & trunk` is *not* an expression.  It's a *definition*
  instead.  But `leaves & trunk` is an expression.

Can you tell the difference?  Expressions describe something, but don't
give it a name.  But every definition has an expression inside, after
the equal sign.  So expressions are pretty important.

### Functions ###

One special kind of expression that you use a lot in CodeWorld is
applying a *function*.  A function is like a variable that still needs
more information.  You've already used a lot of functions:

* `rectangle` is a function.  It needs a width and a height, and makes a
  picture.
* `light` is a function.  It needs a color, and makes another color that's
  about the same, but lighter.
* `pictureOf` is a function.  It needs a picture, and makes a program.
* `scale` is a function.  It needs a picture and two scaling factors, and
  makes a modified picture.

As you've already seen, to apply a function, you can write the function
name, then the extra information it needs (these are called *parameters*)
in parentheses after it, with commas between them.

### Nesting ###

Remember how we used `rotate`?  Here's a quick reminder:

    main    = pictureOf(diamond)
    diamond = rotate(square, 45)
    square  = rectangle(2, 2)

Nice!  However, naming everything like that can get tedious.  If you
have a simple shape, such as `rectangle(2, 2)`, you may not want
to bother giving it a name.  You can just describe the shape right where
the name would go.

Try it:

    main = pictureOf(diamond)
    diamond = rotate(rectangle(2, 2), 45)

Or even:

    main = pictureOf(rotate(rectangle(2, 2), 45))

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

Check out this program:

    main   = pictureOf(design)
    design =   rotate(rectangle(4, 0.2), 1 * 180 / 5)
             & rotate(rectangle(4, 0.2), 2 * 180 / 5)
             & rotate(rectangle(4, 0.2), 3 * 180 / 5)
             & rotate(rectangle(4, 0.2), 4 * 180 / 5)
             & rotate(rectangle(4, 0.2), 5 * 180 / 5)

We could have written `36`, '72', '108', '144', and `180` (the answers to
those math problems).  But this way, it's very clear what we are doing:
dividing 180 degrees into fifths, and then rotating a rectangle by each
amount.  And we don't have to worry about getting one of the answers
wrong!

Just like in math, you can use parentheses to group expressions, so
`3 * (6 - 2)` is `3 * 4`, which is `12`.

### Lists ###

As we explore new kinds of things, we've already seen pictures and numbers.
Now let's look at lists.  A list is, well, just a list of things.  To write
a list, use square brackets around the whole list, and commas between the
things in it.  For example:

* `[ 1, 2, 3, 4 ]` is a list of numbers.
* `[ circle(2), rectangle(3,5), blank ]` is a list of pictures.

You can use the function called `pictures` to combine a list of pictures
together into one picture.  For example:

    main = pictureOf(allThePictures)
    allThePictures = pictures[
        solidRectangle(4, 0.4),
        solidCircle(1.2),
        circle(2)
        ]

#### List comprehensions ####

Lists are a little bit interesting when you use them with `pictures`, but
they get a lot *more* interesting if you write list *comprehensions*.  A
list comprehension is a way to write a list of things by starting with a
simpler list, and copying something once for each thing in it.  So if you
have a list of numbers, you can turn it into a list of *circles*, each
with a different radius.  That looks like this:

    main = pictureOf(target)
    target = pictures[ circle(r) | r <- [1, 2, 3, 4, 5] ]

The list comprehension was `[ circle(r) | r <- [1, 2, 3, 4, 5] ]`, and it
has a few parts:

* The brackets around the whole thing tell you that it's a list. The vertical
  line in the middle separates the left part from the right part.
* On the left side of the vertical line is an expression, that describes the
  elements of the *resulting* list.  Here, each element of the resulting list
  is a circle, but with a different radius.  Notice that we use a *variable*
  called "r" that hasn't been defined yet!
* On the right side of the vertical line, you say what list to start with,
  and name its members.  That's where the variable *r* came from!  You can
  use this new variable on the left side.

So when you look at that list comprehension, it says to start with the list
`[ 1, 2, 3, 4, 5 ]`, and for every number in it, call that number `r`, and
put `circle(r)` in the resulting list.  The resulting list is `[ circle(1),
circle(2), circle(3), circle(4), circle(5) ]`.  But you didn't have to
type that over and over.

#### List ranges ####

With list comprehensions, it's now useful to have lists of numbers, because
you can turn them into lists of pictures.  But writing `[1, 2, 3, 4, 5]` was
still pretty long.  And imagine if you wanted a hundred things, instead of 5!

Luckily, there are easier ways to do it.  if you write `[1 .. 5]`, that's
shorthand for `[1, 2, 3, 4, 5]`.  In the same way, `[3 .. 10]` is shorthand
for `[ 3, 4, 5, 6, 7, 8, 9, 10 ]`.  Much easier!

Even better, if you want to count by 2s or 3s or 10s, you can!.  You just
have to give the first two numbers, then use `..` to continue from there.

Here's another example:

    main = pictureOf(star)
    star = pictures[ rotate(rectangle(10, 1/10), angle)
                     | angle <- [10, 20 .. 360] ]

So we start with a list of number counting by 10s from 10 to 360.  Then we
call each of those numbers the variable "angle", and get a list of pictures
that rotate something by each of those angles.  Finally, we combine all of
this pictures using the `pictures` function, and draw the result.

#### Advanced list comprehensions ####

There are a few other things you can do with list comprehensions.  You
might find them useful.

First, you can filter out certain members of the list you start with.
Suppose you want to draw those circles, like in the `target` example, but
you don't want to draw the middle one.  One way to say that is:

    main = pictureOf(target)
    target = pictures[ circle(r) | r <- [1 .. 5], r /= 3 ]

Notice that `/=` means "not equal to".  So this says to make a picture out
of circles built from each radius from 1 to 5, *except* for 3.

Second, you can include base your list comprehension on several lists.
This will draw a grid of circles:

    main = pictureOf(grid)
    grid = pictures[ translate(circle(1/2), x, y)
                     | x <- [-9 .. 9], y <- [-9 .. 9] ]

Because there are two base lists separated by commas, this will draw a
circle for *every* *possible* *combination* of x and y from those lists.

Another way to use a list comprehension with two lists is to use two
vertical lines.  This is called a *parallel* list comprehension.  Instead
of including a result for all possible combinations, this will only match
the first element of each list, then the second from each list, and so on.
Here's an example, using a list of number, and a list of colors!

    main    = pictureOf(circles)
    circles = pictures[ color(circle(r), c) | r <- sizes
                                            | c <- colors ]
    sizes   = [ 1, 2, 3, 4, 5 ]
    colors  = [ red, green, blue, yellow, purple ]

If you used a comma to separate the base lists, this would draw red,
green, blue, yellow, *and* purple circles at *each* of the sizes, and that
isn't what you want.  By using a parallel list comprehension, you make
sure only the smallest circle is red, then the next smallest is blue, and
so on.

### Points, Lines and Polygons ###

To draw more precise shapes, we can use points on a "coordinate plane".  You
can see a coordinate plane right now, just by running this program:

    main = pictureOf(coordinatePlane)

The coordinate plane is made up of two directions: *horizontal* (also
called x) and *vertical* (also called y).  The very center of the screen
is at position zero in both x and y, and can be written as `(0, 0)`.  In
general, points can be described by listing two numbers:

* Which number they are above or below on the horizontal line.  This is
  called the *x* *coordinate*.
* Which number they are beside on the vertical line.  This is called the
  *y* *coordinate*.

Since zero is in the middle, one direction uses *positive* numbers, and
the other uses *negative* numbers.  Once you have these numbers, you can
write a point by listing them in parentheses with a comma: the x coordinate
*always* comes first, and the y coordinate *always* comes second.

Run the program above, and then try finding these points on the coordinate
plane:

* `(5, 5)`: This is in the top right part of the coordinate plane.
* `(5, 0)`: This is on the middle right.
* `(-5, 5)`: This one is on the top left.  Top because the y coordinate
  (the second number) is positive, and left because the x coordinate (the
  first number) is negative.

Got it?  Great!  Now you can draw things like lines by giving a list of
points in the coordinate plane to a function called `line`:

    main = pictureOf(zigzag)
    zigzag = line[(-2, 0), (-1, 1), (0, -1), (1, 1), (2, 0)]

To draw a closed shape, use `polygon` instead.  Can you figure out the
mystery picture before you run the program?

    main = pictureOf(mystery)
    mystery = line[(-3, -4), (0, 5), (3, -4), (-4, 2), (4, 2), (-3, -4)]

If you prefer to fill in your shape, you can use `solidPolygon` instead of
`polygon` and you'll get a solid version:

    main = pictureOf(mystery)
    mystery = polygon[(-3, -4), (0, 5), (3, -4), (-4, 2), (4, 2), (-3, -4)]

There's also `thickPolygon` and `thickLine` which use an extra parameter
for thickness:

    main = pictureOf(mystery)
    mystery = thickLine(path, 1)
    path = [(-3, -4), (0, 5), (3, -4), (-4, 2), (4, 2), (-3, -4)]

#### Using the coordinate plane to draw ####

A neat trick is to use the coordinate plane as you write your program.  Say
you want to draw a butterfly.  You might start by writing:

    main = pictureOf(butterfly & coordinatePlane)
    butterfly = blank

Now run your program, and you have a coordinate plane to measure what
points to use in your shapes.  When you're done, just remove the
`& coordinatePlane` to get rid of the guidelines.

Types
-----

We've seen many different kinds of things so far that show up in your
programs: pictures, numbers, text, points, colors, lists of all of
these... maybe you're wondering how to keep them all straight!  CodeWorld
calls these kinds of things *types*.  You'll mostly see types in two
places:

* When you make a mistake, you'll often see types mentioned in *error*
  *messages* that tell you about the problem.
* If you want to, you can say things about types in your program.
  If you do, the computer then knows more about what you meant, and
  can sometimes explain the problems in your program better.

### Simple Types ###

Hear are some of the types that you've used in your programs:

* `Program` is the type of the variable `main` that you define in all
  programs.
* `Picture` is the type for pictures.
* `Number` is the type for numbers.
* `Color` is the type for colors.

Notice that while variables start with a lower-case letter, types are
capitalized.

To declare types in your programs, you can use `::`, like this:

    main :: Program
    main = pictureOf(wheel)

    wheel :: Picture
    wheel = solidCircle(size)

    size :: Number
    size = 4

You don't ever have to say what type things are.  The computer can
always figure that out on its own.  But if you do say what your types
are, the computer can often be a lot more helpful in explaining where
your program is wrong.

### List Types ###

What about lists?  Would you get their type is `List`?  Not quite!  There
are many types of lists: lists of numbers, lists of pictures, lists of
colors, and so on.  To write the type of a list, we just write the type
of the things *inside* the list, and surround it with square brackets.

    main = pictureOf(circles)
    circles = pictures[ circle(r) | r <- sizes ]

    sizes :: [Number]
    sizes = [ 1, 2, 3, 4 ]

Notice that I can choose to say the type for `sizes`, even if I don't
give types for anything else.  Again, you can say as little or as much
as you want about types!

### Points and Tuples ###

What about a point, like the ones we used to make lines and polygons?
It actually works just fine to say the type is `Point`:

    main = pictureOf(line[start, end])

    start :: Point
    start = (0, 0)

    end :: Point
    end = (2, -4)

When the compiler talks about points, though, it sometimes calls
their type something different: `(Number, Number)`.  This is just a
way to say what we already know: a point is an ordered pair, with each
part being a number!  It turns out `Point` is just shorthand for
`(Number, Number)`, so they both mean the same thing.

Types like that, with parentheses and commas, are sometimes called
*tuples*.  So a `Point` is a specific kind of tuple.  Other tuples
might use different types, different numbers of things, and even
different types for the different things inside!

* `(Number, Color)` is a tuple.  Some possible values are the pairs
  `(4, red)` or `(-3, dark(green))`.
* `(Number, Text, Number, Color)` is a tuple with four things in it.
  A possible value is `(3, "train", 10, blue)`.

Why would you use these?  Well, they can be useful for list
comprehensions!

    main = pictureOf(boxes)

    boxDetails :: [(Number, Number, Color)]
    boxDetails = [
        (-8, 1, blue),   (-6, 1/2, red), (-1, 3, purple),
        (3, 1/8, brown), (4, 1/2, pink), (8, 2, yellow)
        ]

    boxes = pictures[ translate(color(rectangle(s,s), c), x, 0)
                      | (x, s, c) <- boxDetails ]

See?  You can describe the important characteristics of your picture
in a concise list, and then give the details of how to build the
complete picture later.

### Functions ###

There are also types for functions.  Remember those?  Here are some
examples:

* `light` is a function that needs a color and makes another color.  It
  has the type `Color -> Color`.
* `circle` is a function that needs a number (the radius), and makes a
  picture (a circle with that radius).  It has the type
  `Number -> Picture`.
* `rectangle` is a function that needs two numbers, and makes a picture.
  It has the type `(Number, Number) -> Picture`.
* `translate` is a function that needs a picture and two numbers (the x
  and y distances), and makes a new picture.  It has the type
  `(Picture, Number, Number) -> Picture`.

In general, function types have arrows (`->`).  On the left of the arrow
is the type of things that the function needs.  On the right side of the
arrow is the type of things that the function makes.

Defining Functions
------------------

TODO: Write this section.

### If: Choosing what to do ###

TODO: Write this section.

### Multiple parameters ###

TODO: Write this section.

### Pattern matching ###

TODO: Write this section.

### Recursion ###

TODO: Write this section.

Animations
==========

TODO: Write this section.

    main      = animationOf(design)
    design(t) = rotate(slot, 60 * t) & middle & outside
    slot      = solidRectangle(4, 0.4)
    middle    = solidCircle(1.2)
    outside   = circle(2)

Simulations
===========

Animations are exciting, and get things moving.  But they suffer from one big
drawback: they have no memory!  Every new frame of the program starts over, with
only one number: how many seconds since the start.  You can build animations for
simple motion, where you can immediately describe what it's like at any point as
an expression involving time.  But animations break down when you want something
that acts in a less predictable way.

Think about a ball bouncing around in a room.  Can you write an expression that
says exactly where the ball will be at any point in time?  It's not easy!  But
if you know the position and direction the ball is moving, you can figure out
where it should go next.  This is an example of something that's *hard* to
describe as a function of time, but *easier* to describe the changes happening
in each moment.

Simulations work for precisely this situation.

Parts of a Simulation
---------------------

Pictures and animations were described by a single thing: for a pictures, it was
a picture.  For animations, it was a function mapping numbers to pictures.  But
a simulation is actually built out of *three* separate (but related) parts.
Here they are:

### Part 1: Initial State ###

The first part of a simulation is often called `initial`, and it tells you how
the simulation is when it starts.

### Part 2: Step Function ###

The next part of a simulation is often called `step`, and it tells you how the
simulation changes when time passes.

### Part 3: Draw Function ###

The final part of a simulation is often called `draw`, and it tells you how the
simulation should be presented on the screen as a `Picture`.

All three of these parts share some data called the "world", which records
everything you want to *remember* about the simulation as it happens.

Your First Simulation
---------------------

It may sound complicated, but let's jump in and look at an example:

    main            = simulationOf(initial, step, draw)
    initial(rs)     = (5,0)
    step((x,y), dt) = (x - y*dt, y + x*dt)
    draw(x,y)       = translate(rectangle(1,1), x, y)

In this case, the "world" is a point: the location of an object.  The step
function is the heart of any simulation.  Here, it changes the `x` and `y`
coordinates to move the object, by amounts that depend on where the object is
now.  When the object is near the top of the screen, so `y` is a large number,
it's pushed to the left.  Conversely, when it's near the bottom and `y` is
negative, it's pushed to the right.  When it's near the right, so `x` is a large
number, it's pushed up.  When `x` is negative on the left side, it's pushed
down.

This still leaves `initial`, which tells us that the object starts at (5,0), and
`draw`, which says the object looks like a square, and appears at the position
in the world.

Can you guess what this will look like?  Try it and find out!

Simulations are a lot like science experiments.  You get to describe the rules
for how things change at each moment in time, but the overall behavior of the
system can still surprise you!

Choosing a World
----------------

The world type is the first big choice you make when building a simulation.  In
the simulation above, the world was a point.  That doesn't mean that all your
simulations will be the same.  You should ask yourself: what do you need to
*remember* for the simulation to continue.

Think of these possibilities:
* Do you need to remember the *locations* of things?
* Do you need to remember the *speed* things are moving?
* Do you need to remember the *direction* things are moving?
* Do you need to remember the *angle* of something that turns?

Anything you need to remember will go into your world type.

### Defining New Types ###

TODO: Write this section.

Simulation Tricks
-----------------

### Linear Change ###

TODO: Write this section.

### Position and Velocity ###

TODO: Write this section.

### Constant Acceleration ###

TODO: Write this section.

Randomness
----------

TODO: Write this section.

Interactions
============

TODO: Write this section.

More Information
================

Do you want to know about everything you can use to build your CodeWorld project?
The collection of all variables and types you can use in CodeWorld is called the
*prelude*, and you can look through the whole thing!

> [Show Me the Prelude!][1]

[1]: ./doc/Prelude.html "Prelude API Documentation"
