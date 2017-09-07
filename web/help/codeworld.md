<img src="/codeworld.png" style="max-width: 100%;">

Welcome to CodeWorld!  CodeWorld is an educational computer programming environment.
By using a simple mathematical model for geometric shapes and transformations, you
can create drawings, animations, and even single-player and multi-player video games!
The programming language used is a variation of [Haskell](http://haskell.org), but
you can think of it as just talking to the computer using mathematics.

This is the CodeWorld Guide, which will get you started.  Click outside the guide
to start programming.  The *Guide* button at the bottom of the screen will get you
back here.

Overview
========

To get an idea what you can do in CodeWorld, you might want to check out the
[CodeWorld Gallery](/gallery.html), with examples of projects built by students.

Tips for using this guide
-------------------------

* If you feel the urge to play around with something and try new things, please
  do!  It won't do much good to just read.  You have to try things to learn.
* If you see a blue box, you can click it to copy the code and try it out.

Drawing With CodeWorld
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
"wheel" in that example) *variables*.  So, earlier we defined a *variable* called
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

There are plenty more: `path`, `polygon`, `thickCircle`, `thickRectangle`, `arc`,
`sector`... the list goes on and on!  Don't worry; you will be able to play with
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
  alone.  Numbers bigger than stretch it out, and numbers smaller than 1
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

### Lists ###

As we explore new kinds of things, we've already seen pictures and numbers.
Now let's look at lists.  A list is, well, just a list of things.  To write
a list, use square brackets around the whole list, and commas between the
things in it.  For example:

* `[ 1, 2, 3, 4 ]` is a list of numbers.
* `[ circle(2), rectangle(3,5), blank ]` is a list of pictures.

You can use the function called `pictures` to combine a list of pictures
together into one picture.  For example:

    program = drawingOf(allThePictures)
    allThePictures = pictures([
        solidRectangle(4, 0.4),
        solidCircle(1.2),
        circle(2)
        ])

#### List comprehensions ####

Lists are a little bit interesting when you use them with `pictures`, but
they get a lot *more* interesting if you write list *comprehensions*.  A
list comprehension is a way to write a list of things by starting with a
simpler list, and copying something once for each thing in it.  So if you
have a list of numbers, you can turn it into a list of *circles*, each
with a different radius.  That looks like this:

    program = drawingOf(target)
    target  = pictures([ circle(r) | r <- [1, 2, 3, 4, 5] ])

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

    program = drawingOf(star)
    star    = pictures([ rotated(rectangle(10, 1/10), angle)
                         | angle <- [10, 20 .. 360] ])

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

    program = drawingOf(target)
    target  = pictures([ circle(r) | r <- [1 .. 5], r /= 3 ])

Notice that `/=` means "not equal to".  So this says to make a picture out
of circles built from each radius from 1 to 5, *except* for 3.

Second, you can include base your list comprehension on several lists.
This will draw a grid of circles:

    program = drawingOf(grid)
    grid    = pictures([ translated(circle(1/2), x, y)
                         | x <- [-9 .. 9], y <- [-9 .. 9] ])

Because there are two base lists separated by commas, this will draw a
circle for *every* *possible* *combination* of x and y from those lists.

Another way to use a list comprehension with two lists is to use two
vertical lines.  This is called a *parallel* list comprehension.  Instead
of including a result for all possible combinations, this will only match
the first element of each list, then the second from each list, and so on.
Here's an example, using a list of number, and a list of colors!

    program = drawingOf(circles)
    circles = pictures([ colored(circle(r), c) | r <- sizes
                                               | c <- colors ])
    sizes   = [ 1, 2, 3, 4, 5 ]
    colors  = [ red, green, blue, yellow, purple ]

If you used a comma to separate the base lists, this would draw red,
green, blue, yellow, *and* purple circles at *each* of the sizes, and that
isn't what you want.  By using a parallel list comprehension, you make
sure only the smallest circle is red, then the next smallest is blue, and
so on.

### Points, Lines and Polygons ###

To draw more precise shapes, we can use points on a "coordinate plane".  You
can see a coordinate plane right now, just by running this code:

    program = drawingOf(coordinatePlane)

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

Run the code above, and then try finding these points on the coordinate
plane:

* `(5, 5)`: This is in the top right part of the coordinate plane.
* `(5, 0)`: This is on the middle right.
* `(-5, 5)`: This one is on the top left.  Top because the y coordinate
  (the second number) is positive, and left because the x coordinate (the
  first number) is negative.

Got it?  Great!  Now you can draw things like paths by giving a list of
points in the coordinate plane to a function called `path`:

    program = drawingOf(zigzag)
    zigzag  = path([(-2, 0), (-1, 1), (0, -1), (1, 1), (2, 0)])

To draw a closed shape, use `polygon` instead.  Can you figure out the
mystery picture before you click Run?

    program = drawingOf(mystery)
    mystery = polygon(
        [(-3, -4), (0, 5), (3, -4), (-4, 2), (4, 2), (-3, -4)])

If you prefer to fill in your shape, you can use `solidPolygon` instead of
`polygon` and you'll get a solid version:

    program = drawingOf(mystery)
    mystery = solidPolygon(
        [(-3, -4), (0, 5), (3, -4), (-4, 2), (4, 2), (-3, -4)])

There are also `thickPolygon` and `thickPath` which use an extra parameter
for thickness:

    program = drawingOf(mystery)
    mystery = thickPolygon(
        [(-3, -4), (0, 5), (3, -4), (-4, 2), (4, 2), (-3, -4)], 1)

#### Using the coordinate plane to draw ####

A neat trick is to use the coordinate plane as you write your code.  Say
you want to draw a butterfly.  You might start by writing:

    program   = drawingOf(butterfly & coordinatePlane)
    butterfly = blank

Now run your program, and you have a coordinate plane to measure what
points to use in your shapes.  When you're done, just remove the
`& coordinatePlane` to get rid of the guidelines.

Types
-----

We've seen many different kinds of things so far that show up in your
code: pictures, numbers, text, points, colors, lists of all of
these... maybe you're wondering how to keep them all straight!  CodeWorld
calls these kinds of things *types*.  You'll mostly see types in two
places:

* When you make a mistake, you'll often see types mentioned in *error*
  *messages* that tell you about the problem.
* If you want to, you can say things about types in your code.
  If you do, the computer then knows more about what you meant, and
  can sometimes explain the problems in your code better.

### Simple Types ###

Hear are some of the types that you've used in your code:

* `Program` is the type of the variable `program` that you define in all
  your code.
* `Picture` is the type for pictures.
* `Number` is the type for numbers.
* `Color` is the type for colors.

Notice that while variables start with a lower-case letter, types are
capitalized.

To declare types in your code, you can use `::`, like this:

    wheel :: Picture
    wheel = solidCircle(size)

    size :: Number
    size = 4

You don't *have* to say what type things are.  It's completely optional,
and the computer can always figure that out on its own.  But if you do
say what your types are, two things happen:

* Other people reading your code can understand what's going on.
* When you make a mistake the computer can be more helpful explaining
  what's wrong.

### List Types ###

What about lists?  Would you guess their type is `List`?  Not quite!  There
are many types of lists: lists of numbers, lists of pictures, lists of
colors, and so on.  To write the type of a list, we just write the type
of the things *inside* the list, and surround it with square brackets.

    program = drawingOf(circles)
    circles = pictures[ circle(r) | r <- sizes ]

    sizes :: [Number]
    sizes = [ 1, 2, 3, 4 ]

Notice that I can choose to say the type for `sizes`, even if I don't
give types for anything else.  Again, you can say as little or as much
as you want about types!

### Points and Tuples ###

What about a point, like the ones we used to make paths and polygons?
It actually works just fine to say the type is `Point`:

    program = drawingOf(path[start, end])

    start :: Point
    start = (0, 0)

    end :: Point
    end = (2, -4)

When the computer talks about points, though, it sometimes calls
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

    program = drawingOf(boxes)

    boxDetails :: [(Number, Number, Color)]
    boxDetails = [
        (-8, 1, blue),   (-6, 1/2, red), (-1, 3, purple),
        (3, 1/8, brown), (4, 1/2, pink), (8, 2, yellow)
        ]

    boxes :: Picture
    boxes = pictures([ translated(colored(rectangle(s,s), c), x, 0)
                       | (x, s, c) <- boxDetails ])

You can describe the important characteristics of your picture
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
* `translated` is a function that needs a picture and two numbers (the x
  and y distances), and makes a new picture.  It has the type
  `(Picture, Number, Number) -> Picture`.

In general, function types have arrows (`->`).  On the left of the arrow
is the type of things that the function needs.  On the right side of the
arrow is the type of things that the function makes.

Defining Functions
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
needs on a radius.  `rectangle` needs a width and a height.  Similarly, you
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

### Recursion ###

Recursion means defining something in terms of itself.

This might sound funny, because we normally consider a definition that uses
the same word we're defining to be useless.  You can't define an athlete as
"someone who exercises as much as an athlete", for example.  But when
defining functions, if you are careful, you actually *can* define something
in terms of itself.

A simple example of a recursive function is the factorial function.  It can
be defined like this:

    factorial :: Number -> Number
    factorial(0) = 1
    factorial(n) = n * factorial(n - 1)

Notice that even though `factorial` is defined in terms of itself, it always
has a value for natural numbers, because eventually, everything reduces to
`factorial(0)`, which is not recursive.  Something similar happens for all
recursive functions.  You need:

* A *base* case, which is not defined in terms of itself.
* A *recursive* cases, which is defined in terms of things that are closer
  to the base case.  This way, everything eventually reduces to the base
  case in the end.

A more exciting use of recursive functions is building so-called "fractal"
pictures.  These are pictures that are made up of smaller copies of
themselves.  Here's a simple fractal:

    program = drawingOf(fractal(10))

    fractal :: Number -> Picture
    fractal(0) = stem
    fractal(n) = stem
               & translated(part, 0,  5)
               & translated(part, 0, -5)
      where part = rotated(scaled(fractal(n-1), 2/3, 2/3), 90)

    stem = path([(0, -10), (0, 10)])

There are plenty more kinds of fractals you can build from this same pattern.
The parameter to the function is a level of detail.  Start with a simple
shape as the base case at the lowest level of detail.  Then define the
general shape in terms of smaller shapes at one level of detail lower.

Animations
==========

As you saw in the first part of this guide, the `drawingOf` function can convert
a `Picture` into a program that draws it.  Now you will add some movement to your
programs.  The `animationOf` function is used to create animations.

What is an animation?
---------------------

When you see a television show, movie, or computer game, it looks like things
are moving.  Really, though, you are just looking at still pictures, called frames.
These frames are only slightly different from each other.  Switching from one
frame to the next very quickly creates the illusion of motion.

Have you ever made a flip book?  To make a flip book, you would take a small
notebook, and draw a slightly different picture (a frame!) on each page.  When you
flip through the pages with your thumb, it looks as if the picture is moving, just
like in a movie or video game.  Early animated cartoons were drawn in exactly this
way: artists had to carefully paint many pictures, each one only slightly
different, and put them together to produce the final product.

Computers make this job much easier!  All you need to do is describe a pattern of
motion.  The computer does the hard work of drawing many similar pictures.  The
way you do this is with a function.

    program         = animationOf(propellor)
    propellor(t) = rotated(solidRectangle(10, 1), 60 * t)

See if you can explain to yourself or someone else what is happening here.

### Analyzing the animation ###

The first line asks for an animation, by using the `animationOf` function.  The
parameter is a *function* that can be used to produce the frames of the
animation.  In our example, this function is called `propellor`.

The second line defines the `propellor` function.  The parameter to this function,
which we will usually call `t`, is the time in seconds since the program was
started.  The result of the function should be a picture, which is the frame to
display at that time.  Unlike movies and television shows, which only have a
fixed sequence of frames, computers can work as fast as possible to draw as many
frames as they can, by following the same pattern.

(Have you ever heard of the graphics in video games described in "frames per
second"?  This is, literally, the number of different frames the computer is
capable of drawing in one second.  Slower computers can't draw as many frames,
which can make motion appear jerky and uneven.  Faster computers can draw more
frames, which makes the motion appear smooth and natural.)

The result of the `propellor` function is just an ordinary picture, which one
change.  Instead of a specific angle of rotation, an expression `60 * t` is
used.  Just as with any function, this is evaluated by substitution.  So the
frame that is drawn 4.5 seconds in is rotated by an angle of `60 * 4.5`, which
is 270 degrees.

Try making a table of angles of rotation at each point in time.  How fast (in
degrees per second) is the propellor rotating?

Kinds of motion
---------------

Animations can be built from several kinds of motion.  Anywhere you have used
a number in the description of a picture, you could achieve change over time
by using an expression of `t` instead!

Here are a few possibilities:

* The radius of a circle.
* The width or height of a rectangle.
* The distance by which a shape is translated, scaled, or rotated.
* The angles of an arc or sector.
* The x or y coordinates of points in a path or polygon.
* The bounds of the range used for a list comprehension.
* Red, green, or blue values in a color.

The list goes on and on!  And you don't need to settle for just one of these.
You can use `t` as many times in your animation as you like!  This example
combines rotation, translation, and a list range all depending on the time:

    program   = animationOf(wheels)
    wheels(t) = pictures([
        translated(rotated(tire, -60 * t), t - 10, y)
        | y <- [0, 2 .. t]])
    tire      = circle(1) & solidRectangle(0.1, 2)

Top-down animation
------------------

With drawings, soon after learning how to create a single drawing, you learned
how to build more complicated drawings out of simpler building blocks.  The
same idea applies to animation.  However, it's important to be aware of the
difference between pictures and animations.

As you build up complicated animations, it it crucial to keep in mind exactly
which expressions mean what.  Let's examine a simple animation, such as:

    ball(t) = thickCircle(t, 1)

It is a common mistake to think that because you define the animation by
writing this line, `ball(t)` is the name of the animation.  Actually, `ball`
is the name of the entire animation.  The expression `ball(t)` describes
just one frame of the animation.

To be more complete, here are several expressions that occur in this line,
and their complete types and meanings.

| Expression          | Type                | Meaning                                          |
| ------------------- | ------------------- | ------------------------------------------------ |
| `t`                 | `Number`            | An instant in time.                              |
| `ball`              | `Number -> Picture` | The entire animation.                            |
| `ball(t)`           | `Picture`           | The single frame of `ball` at some point in time |
| `thickCircle(t, 1)` | `Picture`           | The meaning is identical to `ball(t)`            |

Notice that, as always, the equal sign tells you that the expressions on
its left and right sides mean the same thing!  You may wonder, though, if
`ball(t)` is *not* the name of the animation, why you define an animation
by writing `ball(t) = `...  The reason is that to define an animation, you
need to give the pattern that describes each of its frames.

For an analogy, imagine this conversation between a student and a teacher:

> *Teacher:* What does "cousin" mean?
>
> *Student:* Well, if Alice's parent and John's parent are siblings, that
> makes Alice and John cousins.

The student described what cousins are by giving a sentence about two
people named Alice and John.  But the definition wasn't just about
people with those two names.  The names were there because they helped
the student to describe the pattern!  In the same way, when you define
an animation `ball` by talking about a single frame `ball(t)`, your
definition doesn't have anything do with a specific time `t`.  But
having a name for the current time helps in describing the pattern.

We'll refer back to this as we explore some examples of top-down design
with animation.

### Combining animations with `&` ###

The `&` operator is used to combine pictures.  But what if you have two
animations and want to show them at the same time?  You can't use `&` to
combine the two animations.  But you can use `&` to combine two *pictures*,
where those pictures are just one frame of the animation.

So this won't work:

    program = animationOf(a & b)
    a(t)    = rotated(solidRectangle(1, 1), 45 * t)
    b(t)    = circle(t)

But this will work:

    program = animationOf(c)
    c(t)    = a(t) & b(t)
    a(t)    = rotated(solidRectangle(1, 1), 45 * t)
    b(t)    = circle(t)

The first example doesn't work because `a` and `b` are animations rather
than pictures, so they can't be combined using `&`.  But in the second
example, we've instead defined a new animation, `c`, and said that each
frame of `c` is obtained by combining the frames from `a` and `b` at
that time.

Patterns of change
------------------

You have seen how using the parameter, `t`, in an animation can create
motion.  Different patterns or kinds of motion can be created by writing
different kinds of math expressions involving `t`.  In this section, we'll
look at some of these patterns, and what the expression looks like that
creates them.

### Linear change ###

A linear change proceeds at one fixed speed.  A good example of linear change
is a car that moves at a fixed speed across the screen.  Another example is
the propellor above: although the ends of the blade move in a circle rather
than a line, the fundamental change is to an angle, and that angle increases
at a fixed speed.  In this example, the box rotates at a fixed speed of 45
degrees per second, so the change of the angle is linear:

    program = animationOf(box)
    box(t)  = rotated(solidRectangle(1, 1), 45 * t)

When describing linear change, there are two questions to ask yourself:

* What is the speed of change?
* What is the starting value?

You get the expression for linear change by *multiplying* `t` by the speed,
and *adding* the starting value.  For example, a starting value of `7` and
a speed of `-3` would give the expression `-3 * t + 7`.

### Periodic change ###

Periodic change happens in a repeating cycle.  One example is a pendulum,
which moves back and forth in the same motion forever.  We create this
pattern of motion using a special function called a sine wave, and
written as `sin`.  Here's a simple pendulum for an example:

    program     = animationOf(pendulum)
    pendulum(t) = rotated(arm, 45 * sin(60 * t))
    arm         = translated(solidRectangle(1, 6), 0, -3)
                & translated(solidCircle(1), 0, -6)

Periodic change is a little more complicated than linear motion.  There are
four questions you need to ask yourself to plan this motion.

* How much does it change?  This is the *amplitude*.
* What is the center, or *resting value*, of the change?
* How quickly does the cycle repeat?  This is the *frequency*.
* At what point in the cycle does it start?  This is the *phase*.

The first two questions are answered with values.  If you want a value to
change periodically between 0 and 10, then you would choose a resting value
of `5`, which is the center of the range.  The amplitude would also be `5`,
since that's the range of motion away from the center.

The second two questions are answered with angles, in degrees.  A full cycle
is 360 degrees.  With this in mind, the frequency is the degrees per second
of progress through the cycle.  For example, if you want one cycle per second,
then you need a frequency of 360 degrees per second.  The phase is the
starting point.  In many cases, you won't care about the phase.

Once you have these four values, you'll combine the `sin` function with
*two* linear expressions.  The frequency and phase form a linear expression
in the parameter to `sin`, and the amplitude and resting value make a linear
pattern with its result.  Putting it all together, the expression looks like
`amplitude * sin(frequency * t - phase) + restingValue`.

### Quadratic change ###

Quadratic change continually speeds up or slows down at a constant rate.  The
rate at which it speeds up or slows down is called *acceleration*.  An example
is a ball thrown in the air.  It will gradually slow down over time, until it
stops and then falls back down, due to acceleration caused by gravity.

Here's an example of a ball flying through the air using quadratic change:

    program = animationOf(ball)
    ball(t) = translated(solidCircle(1),
                         10 * t - 10,
                         -5 + 20 * t - 10 * t^2)

Notice that the x coordinate is a linear expression, because the ball moves at
a fixed speed in that direction.  But the y direction involves gravity.

The three questions to ask where are:

* What is the *acceleration*, or change in speed?
* What is the *starting speed*?
* What is the *starting value*?

The starting value is the plain number in the expression.  The starting speed
is multiplied by `t`.  Finally, half of the acceleration is multiplied by
`t^2`.  You can modify these numbers in the example above to find out what
happens if the starting speed is different, or even if you change the
acceleration.  In this example, changing the acceleration is like adjusting
the strength of gravity, so you can try out the ball on the moon... or on
Jupiter, which has more gravity than the Earth!

### Piecewise motion ###

Still more motion follows different patterns at different times.  When there
are several distinct steps to the change in an animation, we call it "piecewise"
because it have distinct pieces, which are different from each other.

You can describe piecewise motion in your programs using functions like `min` and
`max` and `remainder`, or by using conditionals, like `if` or guards.

The `min` and `max` functions take the minimum or maximum, respectively, of two
numbers.  This allows you to create simple effects where something moves and
then stops.  For example:

    program = animationOf(drop)
    drop(t) = translated(ball, 0, max(-8, 10 - t))
    ball    = solidCircle(1)

The ball will be at a y position of `10 - t` as long as that's more than `-8`.
But as soon as the ball reaches `-8`, it stops, because the first parameter to
`max` is now larger.  You would use `min` for the opposite situation, to model
a number that increases up to a limit.

The `remainder` function causes a number to increase up to a limit, then drop
back down to zero and start over.  For example:

    program   = animationOf(twitch)
    twitch(t) = rotated(solidRectangle(5, 1), remainder(45 * t, 90))

The expression `45 * t` is linear, so it increases steadily at a rate of 45
degrees per second.  But the remainder function ensures that when it reaches
30, it jumps back to zero.  The result is a motion that increases for 30
degrees, then falls back down.

#### Using if, then, and else ####

One more flexible way to define piecewise functions is using `if`, `then`, and
`else`.  Here's an example:

    program         = animationOf(trafficLight)
    trafficLight(t) = colored(dot, if t < 5 then red else blue)

The expression `if c then a else b` means to evaluate whether `c` (called the
"condition") is true or false.  If it's true, then `a` is the result.
Otherwise, `b` is the result.  In the example, the condition is `t < 5`.  If
this is true, then the color is `red`, and if not, it's `blue`.

#### Guards ####

Another way to define a function piecewise is easier to use when there are
more than a couple options.  Guards are used to give several different
definitions for an entire function, each attached to some condition that
days when it applies.

Here's an example of an animation using guards.

    program       = animationOf(flight)

    flight(t)
      | t <  2    = translated(rocket, -5, 2 * t)
      | t <  5    = translated(rotated(rocket, 60 - 30 * t), -5, 4)
      | t < 10    = translated(rotated(rocket, -90), 2 * t - 15, 4)
      | t < 13    = translated(rotated(rocket, 15 * t - 240), 5, 4)
      | otherwise = translated(rotated(rocket, -45), 2 * t - 21, 2 * t - 22)

    rocket = solidRectangle(1, 4)

Instead of the equal sign and a right-hand side, this function has a number
of cases.  Each case has a vertical line, the condition, an equal sign, and
the definition that applies *only* if that condition is true.  The function
always chooses the first case which matches the condition.  The last case,
labeled as `otherwise`, applies when none of the earlier cases match.

#### The multi-step animation pattern ####

The example above with guards works, but it is very difficult to read or
write.  Here's the problem: if `t` is 11, then the first case that will match
is `t < 13`, which describes the motion between 10 and 13 seconds.  *But* `t`
is still 11, which is the time relative to the beginning of the program.  So
the "starting point" values in expressions with `t` have to be written in
terms of the start time of the entire program!

In practice, you almost always want to think of each *step* of your program
as starting from a time of zero.  One way you can do that is to write each
step as a function, and give it the right value for its own time when it's
used.

Here, we've rewritten the rocket example in this way:

    program       = animationOf(flight)

    flight(t)
      | t <  2    = step1(t)
      | t <  5    = step2(t - 2)
      | t < 10    = step3(t - 5)
      | t < 13    = step4(t - 10)
      | otherwise = step5(t - 13)

    step1(t) = translated(rocket, -5, 2 * t)
    step2(t) = translated(rotated(rocket, -30 * t), -5, 4)
    step3(t) = translated(rotated(rocket, -90), 2 * t - 5, 4)
    step4(t) = translated(rotated(rocket, 15 * t - 90), 5, 4)
    step5(t) = translated(rotated(rocket, -45), 2 * t + 5, 2 * t + 4)

    rocket = solidRectangle(1, 4)

This code does exactly the same thing as the earlier example.  But see
how much easier it is to understand?  For example, in `step5`, the starting
points for the translation are `5` and `4`, which are the same as the
translations from the previous step.  Arranging for the animations to match
was much easier this time.

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

### Part 1: Initial state ###

The first part of a simulation is often called `initial`, and it tells you how
the simulation is when it starts.

### Part 2: Step function ###

The next part of a simulation is often called `step`, and it tells you how the
simulation changes when time passes.

### Part 3: Picture function ###

The final part of a simulation is often called `picture`, and it tells you how the
simulation should be presented on the screen as a `Picture`.

All three of these parts share some data called the "world", which records
everything you want to *remember* about the simulation as it happens.

Your First Simulation
---------------------

It may sound complicated, but let's jump in and look at an example:

    program          = simulationOf(initial, step, picture)
    initial(rs)      = (5, 0)
    step((x, y), dt) = (x - y * dt, y + x * dt)
    picture(x, y)    = translated(rectangle(1, 1), x, y)

In this case, the "world" is a point: the location of an object.  The step
function is the heart of any simulation.  Here, it changes the `x` and `y`
coordinates to move the object, by amounts that depend on where the object is
now.  When the object is near the top of the screen, so `y` is a large number,
it's pushed to the left.  Conversely, when it's near the bottom and `y` is
negative, it's pushed to the right.  When it's near the right, so `x` is a large
number, it's pushed up.  When `x` is negative on the left side, it's pushed
down.

This still leaves `initial`, which tells us that the object starts at (5,0), and
`picture`, which says the object looks like a square, and appears at the
position in the world.

Can you guess what this will look like?  Try it and find out!

Simulations are a lot like science experiments.  You get to describe the rules
for how things change at each moment in time, but the overall behavior of the
system can still surprise you!

Choosing a World
----------------

The world type is the first big choice you make when building a simulation.  In
the simulation above, the world was a point.  When you write your own simulations,
though, the world type can be whatever you choose.  The world type is your answer
to this question:  What do I need to *remember* for the simulation to continue.

Think of these possibilities:
* Do you need to remember the *locations* of things?
* Do you need to remember the *speed* things are moving?
* Do you need to remember the *direction* things are moving?
* Do you need to remember the *angle* of something that turns?

Anything you need to remember will go into your world type.  Let's look at some
examples.

### Simulations With one number

The simplest kind of state is a single number.  Let's build a simulation to move
a box across the screen.  You could have done this with an animation, but this
makes a good starting point to learn how things work with simulations.

    program     = simulationOf(initial, step, picture)
    initial(rs) = -10
    step(x, dt) = x + dt
    picture(x)  = translated(solidRectangle(1, 1), x, 0)

The world, in thic coordinate, is the x coordinate of the box.  That's the only
thing you need to remember!  Why not the y coordinate?  Because it is always
zero, so it doesn't change.

Now that we know what the world looks like, let's examine the three parts of
this simulation.  The initial x coordinate is -10, meaning the box starts on the
left edge of the screen.  When time passes, the amount of time passed is added to
the x coordinate, to produce a new x coordinate.  So the x coordinate will
increase at a rate of one unit for every one second.  The program looks like a
box, translated by the x coordinate, and 0 in the y direction.

Try this out, and verify that it does what you expect.

* Can you modify the code to move the box up?  What about left or down?
* Can you modify the code to use rotation instead of translation?
* Can you change the speed at which the box moves?  (Hint: use multiplication.)

### Simulations With multiple numbers

Adding a second number to the state lets you describe different kinds of change
at the same time.  Let's try to make a rolling wheel, which will need to move
(translation) and turn (rotation) at the same time.  In the state, we'll need
both an x coordinate, and an angle of rotation.

    program = simulationOf(initial, step, picture)
    initial(rs) = (-10, 0)
    step((x, angle), dt) = (x + dt, angle - 60 * dt)
    picture(x, angle) = translated(rotated(wheel, angle), x, 0)
    wheel = circle(1) & solidRectangle(2, 1/4) & solidRectangle(1/4, 2)

The initial world has an x coordinate of -10, and an angle of zero.  As time
passes, the x coordinate increases by one unit per second, but the angle of
rotation decreases by 60 degrees per second.  The screen looks like a wheel,
rotated by the angle, and translated by the current x coordinate.

* What happens if you add `60 * dt` instead of subtracting?
* What goes wrong if you use a different number for degrees per second?

If you were wondering, the angle of 60 was chosen by experimenting!  The right
speed for rotation depends on how fast the wheel is moving in the x direction
and the size of the wheel.  Here, a little math would tell you that the right
answer is 180 divided by pi, or about 57.3 degrees.  But 60 is close enough!

### Invisible state ###

So far, everything that has been part of the world has been visible in the
picture.  That's not always true, though!  In fact, it's rarely true for more
advanced animations.  Some things need to be remembered to draw to the screen,
but other things - like speeds, time remaining, etc. - need to be remembered
only because they affect how other things change in the future.

This simple animation shows a baseball flying through the air.  While only
the x position and y position are needed to *draw* the simulation, the y
speed also changes, and needs to be remembered.

    program = simulationOf(initial, step, picture)
    initial(rs) = (-9, -9, 15)
    step((x, y, vy), dt) = (x + 6 * dt, y + vy * dt, vy - 10 * dt)
    picture(x, y, vy) = translated(solidCircle(1/2), x, y)

As you can see, the velocity in the y direction, `vy`, starts at 15 units per
second upward.  But as time passes, it decreases, until eventually it becomes
negative, and the ball falls back down again.

* What would happen if the ball started at a different position?
* How would you change the effect of gravity to simulate a ball on the moon?

Simulating Physics
------------------

### Linear Change ###

TODO: Write this section.

### Position and Velocity ###

TODO: Write this section.

### Gravity and Constant Acceleration ###

TODO: Write this section.

Designing With Simulation
-------------------------

TODO: Write this section.

### Horizontal Composition

TODO: Write this section.

### Vertical Composition

TODO: Write this section.

Defining Types
--------------------

TODO: Write this section.

Randomness
----------

TODO: Write this section.

Interactions
============

TODO: Write this section.

Advanced Examples
=================

More examples of CodeWorld can be found on the example page [here!](?help/examples.md).

More Information
================

Do you want to know about everything you can use to build your CodeWorld project?
The collection of all variables and types you can use in CodeWorld is called the
*prelude*, and you can look through the whole thing!

> [Show Me the Prelude!][1]

[1]: ./doc/Prelude.html "Prelude API Documentation"
