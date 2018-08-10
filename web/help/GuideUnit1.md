Getting started
===============

To build your first program in CodeWorld, type (or just click on) this
code:

~~~~~ . clickable
program = drawingOf(codeWorldLogo)
~~~~~

If you type this and click the **Run** button, you'll see a drawing of the
CodeWorld logo in the *canvas* on the right side of the screen.

!!! Warning: Did something go wrong?
    If you don't see the CodeWorld logo, don't worry.  You can fix it!

    Computers can be very picky, so make sure you have typed *exactly* what
    you see above.  That means everything needs to be spelled correctly, the
    same letters need to be capitalized, and you need parentheses just where
    they are.  There should be nothing else in your editor except that one
    line.

    When you make a mistake, the computer usually shows you an *error message*,
    which shows up on the right side of your screen, in a pink shaded window.
    These messages tell you what went wrong.

    ![](/help/cw-error-notinscope.png)

    * If you see "not in scope", this means you have misspelled a word, or
      used the wrong capitalization.
    * If you see "parse error", this can mean you've left out or have
      extra punctuation marks or symbols.

    Check your code carefully, and try again.

Dissecting your first program
-----------------------------

If you've done coding before, you may have heard that code is like a recipe
that tells a computer step-by-step what to do.  In CodeWorld, though, code is
more like a *dictionary* or *glossary*. It tells the computer what words mean.

Here's the code you just wrote, and what its parts mean.

    | `program`  | `=` | `drawingOf(` | `codeWorldLogo`     | `)` |
    |------------|-----|--------------|---------------------|-----|
    | My program | is  | a drawing of | the CodeWorld logo. |     |

* `program` is the **variable** that you're defining. A variable is a name for
  something.  In most math, variables are just one letter long, and they stand
  for numbers.  In CodeWorld, though, variables can name many different types of
  values: numbers, pictures, colors, text, and even whole programs.  Because
  you will use so many of them, you can name variables with with whole words,
  always starting with a *lower-case* letter.

!!! collapsible: Camel case
    Sometimes, you may want more than one word to name a variable!
    The computer needs each variable to be a single word starting with a
    lower-case letter--so leave out the spaces.  To make it easier to
    tell when a new word starts, you can capitalize the *second* and
    *later* words.

    ![](/help/camel.png width="40%")

    In your first programs, `drawingOf` and `codeWorldLogo` were written in
    this way.  It's often called **camel case**.  Why?  Because the variable
    name has humps!

* The **equal sign** means "is", and tells the computer that two expressions mean
  the same thing.  It is used to connect a variable with its definition.

* `drawingOf` is called a **function**.  You'll use functions a lot, and you'll
  learn more about them later!  This particular function, `drawingOf`, tells the
  computer that your program is a drawing.  So instead of playing a game or
  animation, it will just show a picture.

* `codeWorldLogo` is the specific picture your program will show.  Your
  computer already knows what the CodeWorld logo looks like, so this was an
  easy program to write.  Most of your own programs will need to describe
  the picture that you want to show in more detail.

Building a nametag
------------------

Of course, you can do a lot more in CodeWorld than just look at the CodeWorld
logo!  Next, you can build a digital nametag for yourself.  To do this,
you'll start by telling your computer that your program should be a drawing
of a nametag.

~~~~~ . clickable
program = drawingOf(nametag)
~~~~~

**This program doesn't work!**  If you've typed everything correctly you should
see an error message, `Variable not in scope: nametag :: Picture`.
This is your computer telling you that it doesn't know what `nametag` means!

!!! Tip: Run your program often
    Even though your code was not finished, it didn't hurt to click **Run**.
    You can think of error messages as being a conversation with your
    computer.  You're just asking what it needs next!

    In this case, `Variable not in scope` told you that you need to
    define `nametag`.  But if you typed something wrong, you might see
    a different message, like `Parse error` or `Couldn't match types`.
    These are clues that you have made a different mistake.  That's
    okay, too.  The sooner you know about the mistake, the sooner you
    can fix it.

    Did you notice that your computer already told you that nametag is a
    picture?  It figured that out from context.  Because you asked for a
    *drawing* of `nametag`, it is expecting `nametag` to be a picture.
    Just one more way the computer lets you know what it's thinking.

To finish your code, you'll need to define the variable `nametag`, and
describe to your computer exactly what a nametag is.  To start, you can
add your name, like this!

~~~~~ . clickable
program = drawingOf(nametag)
nametag = lettering("Camille")
~~~~~

You've used a new function, **`lettering`**.  This function describes a
picture with letters (or any other kind of text) on it.

Next, you can add a border to your nametag.  You might be tempted to add
a new line like `nametag = ...` to your code, but you can't! Remember,
your code is like a dictionary, and each definition in it should give
the whole definition for that word.  To include a second shape in your
nametag, you'll use **`&`**, which you can read as "and" or "in front
of". To describe the border itself, two more functions -- **`circle`**
and **`rectangle`** -- are useful.

Here's a name tag with a border:

~~~~~ . clickable
program = drawingOf(nametag)
nametag = lettering("Camille") & circle(4) & rectangle(8, 8)
~~~~~

Here are the shape functions you can use in your nametag, and the
**arguments**, or information inside the parentheses, that each one
expects.

| Function    | Expected arguments (inside parentheses)            | Example            |
|-------------|----------------------------------------------------|--------------------|
| `lettering` | Some text in quotation marks                       | lettering("Jenna") |
| `circle`    | A radius--the distance from the center to the edge | circle(7)          |
| `rectangle` | The width *and* height of the rectangle            | rectangle(5, 3)    |

Try these examples to learn more:

!!! : Concentric circles
    ~~~~~ . clickable
    program = drawingOf(nametag)
    nametag = lettering("Diego") & circle(4) & circle(5) & circle(6)
    ~~~~~

    The `circle` function needs only a single number, which is the *radius*
    of the circle.  Radius means the number of units from the center of
    the circle to the edge.

!!! : Nested rectangles
    ~~~~~ . clickable
    program = drawingOf(nametag)
    nametag = lettering("Alyssa") & rectangle(6, 2) & rectangle(7, 3)
    ~~~~~

    The `rectangle` function needs **two** numbers.  The first is how many
    units wide it should be, and the second is how many units tall.

!!! : Overlapping rectangles
    ~~~~~ . clickable
    program = drawingOf(nametag)
    nametag = lettering("Karim") & rectangle(8, 2) & rectangle(7, 3) &
              rectangle(6, 4) & rectangle(5, 5)
    ~~~~~

    Notice how the definition of `nametag` got too long?  It's okay to
    start a new line, but you **must** indent the new line.  That is,
    leave some blank spaces at the beginning.  If you forget to indent,
    the computer will be confused, and think you're defining a new
    variable.  This can cause a `Parse error` message.

Once you've understood these examples, try your own combinations, as well.

Understanding mistakes
----------------------

As you experminent with your programs, you're likely to try a few things
that don't work.  Explore these common error messages to understand what
went wrong.

!!! collapsible: `Parse error: naked expression at top level`
    This message means there's something in your program that doesn't look
    like a definition.  For example, if you wrote this:

    ~~~~~
    program = drawingOf(nametag)

    lettering("Jonas") & circle(5)
    ~~~~~

    On the second line, instead of an *equation* to define `nametag`, this
    program just wrote an expression.  Remember that your code is a
    glossary, and everything in it should define a word.  To correct this
    mistake, you would add `nametag =` to the beginning of the second line.

    ~~~~~ . clickable
    program = drawingOf(nametag)

    nametag = lettering("Jonas") & circle(5)
    ~~~~~

!!! collapsible: `parse error (possibly incorrect indentation or mismatched brackets)`
    This message can come from a missing *indent*.  For example, consider this code:

    ~~~~~
    program = drawingOf(nametag)

    nametag = lettering("Emma") &
    circle(10)
    ~~~~~

    Here, the programmer meant for `circle(10)` to be part of the
    definition of `nametag`.  The computer doesn't understand this, and
    believes it should be a new definition, because it starts against the
    left margin.  To fix this mistake, you would add a few spaces at the
    beginning of the last line, to indent it.

    ~~~~~ . clickable
    program = drawingOf(nametag)

    nametag = lettering("Emma") & circle(10)
    ~~~~~

    This error can also tell you that you have an open parethesis -- **(** --
    without a matching close parenthesis -- **)**.

!!! collapsible: `Multiple declarations of nametag`
    This message tells you that you defined the same word in two different
    ways.  Sometimes this happens in human languages, and we call them
    homonyms -- for example, "duck" could mean an animal, or it could mean
    dodging a flying object!  Your computer, though, can't deal with this
    each variable can only be defined once.

    Take a look at this program.

    ~~~~~
    program = drawingOf(nametag)

    nametag = lettering("Victor")
    nametag = circle(10)
    ~~~~~

    One line says `nametag` means the text "Miriam", but the second line
    says that `nametag` instead means a circle with a radius of 10.  So
    which is it?  Most likely, the programmer here meant for `nametag` to
    include both.  To do that, you would write one definition for
    `nametag`, and use **`&`** to combine the parts.  Like this:

    ~~~~~ . clickable
    program = drawingOf(nametag)

    nametag = lettering("Victor") & circle(10)
    ~~~~~

!!! collapsible: `Couldn't match type Program with Picture`
    Once you've got the hang of using **`&`** to combine pictures, you
    might be tempted to use it everywhere! For example, you might try to
    write this.

    ~~~~~
    program = drawingOf(nametag) & drawingOf(border)

    nametag = lettering("Miriam")
    border = circle(10)
    ~~~~~

    The problem here is that **`&`** can only combine *pictures*.  The
    `drawingOf` function turns a picture into a *computer program*, and
    you cannot use **`&`** to combine two different computer programs!

    The solution here is to combine the pictures, before using the
    `drawingOf` function.  That looks like this.

    ~~~~~ . clickable
    program = drawingOf(nametag & border)

    nametag = lettering("Miriam")
    border = circle(10)
    ~~~~~

!!! collapsible: `Couldn't match type Picture with Text`
    Just like there's a difference between a computer program and a
    picture, there's also a difference between *text* and *pictures*.
    Consider this code.

    ~~~~~
    program = drawingOf(nametag)
    nametag = "Haruto" & circle(10)
    ~~~~~

    The text, "Haruto", isn't a picture yet, so it cannot by combined
    using **`&`**.  Use the `lettering` function to exchange the text
    for a picture first, then combine it.  Like this:

    ~~~~~ . clickable
    program = drawingOf(nametag)
    nametag = lettering("Haruto") & circle(10)
    ~~~~~

Regardless of the message, you can always click on the line and column
number next to an error message, and get straight to the location in
your code where the error was recognized.  (Sometimes, though, your
actual mistake could be earlier!)  So just read over it and double-check
that you've typed what you intended.

Defining variables
------------------

In the nametags above, you defined *variables* called `program` and `nametag`.
Because your code is like a dictionary or glossary, you can define as many
variables as you like. For example, you might write:

~~~~~
name = "Han"
age = 14
favoriteColor = blue
~~~~~

!!! Warning
    This isn't a complete program!  If you try to run this example, you will see
    an error message:

    `The variable program is not defined in your code.`

    More on that later!

Each of these lines is an **equation**, which says that two expressions are
*equal*, or have the same value.  In math, you use equations in many
ways, but in CodeWorld they are used specifically to *define variables*.

When you define a variable, you can use it in the rest of your code, like
this:

~~~~~ . clickable
program = drawingOf(nametag)
nametag = lettering(name) & circle(4)
name = "Guiseppe"
~~~~~

This code says that your program is a drawing of a nametag, a nametag
contains lettering of the name, and the name is "Guiseppe".  So "Guiseppe"
is written on the name tag.

!!! Warning
    Don't put quotes around a variable!
    This code includes the expression `lettering(name)`, **without**
    quotation marks.  What would happen if you included quotation marks?
    You'd see a nametag with the word "name" written on it!

    Oops!  Quotation marks tell the computer *not* to interpret something
    as code, but just as a piece of text.  Variables are code, so don't
    put quotation marks around your code.

Remember, though, that defining a variable doesn't do anything by itself.
Suppose you wrote this code:

~~~~~ . clickable
program = drawingOf(nametag)
nametag = lettering("Chris")
border = circle(5)
~~~~~

If you run the code, you might be surprised to find there is no border!
You've told your computer what the word `border` means, but you didn't
say you wanted one in your program!  You might try this instead.

~~~~~ . clickable
program = drawingOf(nametag)
nametag = lettering("Chris") & border
border = circle(5)
~~~~~

That extra `& border` tells your computer that you actually *want* a
border in the name tag.  Defining it isn't enough.

### The `program` variable

Remember that defining a variable doesn't do anything by itself.  But your
code is nothing but a bunch of definitions, just like a glossary or
dictionary! Then how does your code make any difference at all?

The answer lies in a special variable called `program`.  Every CodeWorld
project needs *exactly one* definition for the `program` variable.
This is where the computer will look for a description of the program it
should run.  Sometimes, like in the very first program you wrote, this
is the only definition you need.  But usually, you will use other
variables that the computer doesn't know *in* your definition of
CodeWorld. Just like you might if you were studying, the computer will look
up those words, too. And if their definitions use more words the computer
doesn't know, it will look up *those* words, and so on until it
understands everything.

Remember: a definition only matters if the variable you're defining is
*used* somewhere in the definition of program (or the definition of
something else used in program, or so on.)

!!! Tip: Warnings
    When you write code that is correct, but could be improved, you will
    sometimes see a *warning*.  This is a message in the message area
    that lets you know about a problem, even though your program runs.

    One warning you might see is `defined but not used`.  This warning
    tells you that you've defined a variable, but it's not necessary,
    because it isn't used anywhere in the meaning of that special
    `program` variable.

All about functions
-------------------

All of the code you've written so far has made use of functions.  These are
important tools you'll use in your coding, so let's investigate them in more
detail.

> A **function** is a relationship that associates each possible input with a
> specific result.

The type of input a function expects is called its *domain*, and the type of
result is called its *range*.  Here are the functions you've used so far.

Function    | Domain             | Range
------------|--------------------|---------
`drawingOf` | `Picture`          | Program
`lettering` | `Text`             | Picture
`circle`    | `Number`           | Picture
`rectangle` | `(Number, Number)` | Picture

(In addition to this, the **`&`** is a binary operator.  It's a kind of
function, too, but it works differently because it is a symbol, rather than
a name.  Since it works differently, don't think of it as a function quite
yet.)

**Applying** a function means using the function to describe its result for
a specific input.  To apply a function to some input, you write the name of
the function, an open parenthesis, the input values (called **arguments**),
and then a close-parenthesis.  When there is more than one argument to a
function, you can write the domain in CodeWorld by listing them all in
parentheses, separated by commas, like you see in `rectangle` in the table
above.

!!! Tip
    Parentheses always come in pairs, and are used like a circle around the
    arguments to a function.  The name of the function, at the beginning,
    adds a handle to that circle, forming something like a frying pan.

    ![](/help/cw-frying-pan.png)

    Go over some programs you've written up to this point, and see if you
    can see the frying pans for every function that's used.

### Exploring available functions

How you can use a function in your code depends on its domain and range.
There's a short notation that's useful for saying what the domain and range
of a function are, and it looks like this:

~~~~~
drawingOf :: Picture -> Program
lettering :: Text -> Picture
circle :: Number -> Picture
rectangle :: (Number, Number) -> Picture
~~~~~

These lines, called **type signatures**, don't *define* the functions; they
just provide a little bit of information about what arguments they need,
and what type of result they have.  Read the two colons (**`::`**) as
"has the type", and the arrow (**`->`**) as meaning a *function* from one
kind of thing to another.  So the first line says "`drawingOf` has the type:
function from pictures to programs".

You can explore all of the functions the computer already knows when you
use CodeWorld, by pressing Shift-Space or Ctrl-Space on a blank line in the
CodeWorld editor.  Try it!  The list gives all the names your computer
already knows, and there are a lot of them!  By typing the first few
letters of the function you want, you can narrow down the list.  The type
signatures tell you what types of information you need to provide to apply
the function, and what type you can expect to end up with.

For practice, see if you can write code using each of the following
functions.  Start by looking up their domain and range using Shift-Space or
Ctrl-Space, and see if you can use them with just that hint.  If you need
more hints, expand the sections below for an example and explanation.

!!! collapsible: `solidCircle`
    ~~~~~
    solidCircle :: Number -> Picture
    ~~~~~

    This type signature tells you that the only argument to `solidCircle`
    is a number, and the result is a picture.  Here's an example of a
    program that uses `solidCircle`.

    ~~~~~ . clickable
    program = drawingOf(pic)
    pic = solidCircle(5)
    ~~~~~

    You might have noticed that even though the type signature tells you
    the input is a number, it doesn't tell you what that number means!
    Type signatures just tell you the *form* needed to use the function,
    not the meaning.  But if you experiment, you may discover that the
    argument is the radius of the circle.

!!! collapsible: `solidRectangle`
    ~~~~~
    solidRectangle :: (Number, Number) -> Picture
    ~~~~~

    This type signature tells you that `solidRectangle` needs two
    arguments, both numbers, and the result is a picture.  Here's an
    example of a program that uses `solidRectangle`.

    ~~~~~ . clickable
    program = drawingOf(pic)
    pic = solidRectangle(7, 3)
    ~~~~~

    The first number is the width of the rectangle, and the second
    number is the height.

!!! collapsible: `thickCircle`
    ~~~~~
    thickCircle :: (Number, Number) -> Picture
    ~~~~~

    This type signature tells you that `thickCircle` needs two arguments,
    both numbers, and the result is a picture.  Here's an example of a
    program that uses `thickCircle`.

    ~~~~~ . clickable
    program = drawingOf(pic)
    pic = thickCircle(5, 1)
    ~~~~~

    Functions beginning with "thick" draw shapes with a thick line, and
    the thickness of that line is the last argument to the function.  So
    The first argument is the radius of the circle, and the second is
    the line thickness the circle is drawn at.

!!! collapsible: `thickRectangle`
    ~~~~~
    thickRectangle :: (Number, Number, Number) -> Picture
    ~~~~~

    This time, the type signature tells you that `thickRectangle` needs
    three arguments, a new record!  All three arguments are numbers, and
    the result is a picture.  Here's an example of a program that uses
    `thickRectangle`.

    ~~~~~ . clickable
    program = drawingOf(pic)
    pic = thickCircle(8, 4, 1)
    ~~~~~

    The first two arguments are the width and height of the rectangle.
    The third and final argument is the thickness of the line to draw it
    with.

!!! collapsible: `codeWorldLogo`
    ~~~~~
    codeWorldLogo :: Picture
    ~~~~~

    This was a trick question: `codeWorldLogo` isn't a function at all!
    It still has a type signature, but there is no arrow, because it's
    just a picture.  That means there are no parentheses after it, and
    no arguments.

    ~~~~~ . clickable
    program = drawingOf(pic)
    pic = codeWorldLogo
    ~~~~~

    This is the same program as the very first one you wrote!

As you continue with CodeWorld, you'll learn a few more functions.  For
now, see if you can spruce up your nametag with some thicker lines.
Try some solid shapes, too, and think about whether they are useful for
creating your nametag.

Transformations
===============

Colors
------

Pictures don't need to be black and white.  You can use `colored` to change the color
of your pictures.  Here's a simple example:

~~~~~ . clickable
program  = drawingOf(redWheel)
redWheel = colored(wheel, red)
wheel    = solidCircle(4)
~~~~~

You can also mix colors in the same picture:

~~~~~ . clickable
program = drawingOf(tree)
tree    = colored(leaves, green) & colored(trunk, brown)
leaves  = sector(0, 180, 4)
trunk   = solidRectangle(1, 4)
~~~~~

You can also modify the colors!  Here are a few ways to do that:

* `dark(red)`, means `red`, except a little darker.
* `light(green)` means `green`, but a little lighter.  So `light` is the opposite
  of `dark`.
* `translucent(blue)` means blue, but see-through.  The word "translucent" means
  partially transparent or see-through.

Let's try some example code:

~~~~~ . clickable
program = drawingOf(overlap)
overlap = colored(square,  translucent(blue))
        & colored(disk, translucent(green))
square  = solidRectangle(5, 5)
disk    = solidCircle(3)
~~~~~

Transformations
---------------

So far, all the pictures we've drawn have been at the middle of the screen.  That's
no fun.  But never fear, *transformations* are here!

Transformations are ways to change a picture.  There are three kinds of
transformations you can use in CodeWorld:

### Translation: Moving Your Pictures ###

You can *translate* a picture to move it up, down, left, or right on the screen.
To use `translated`, you give it three things:

* A picture to move.
* A distance to move the picture *left* or *right*. Negative numbers
  are left, and positive numbers are right.
* A distance to move the picture *up* or *down*.  Negative numbers are down,
  and positive numbers are up.

Ready for an example?

~~~~~ . clickable
program = drawingOf(forest)
forest  = translated(tree, -5, 5)
        & translated(tree,  0, 0)
        & translated(tree,  5,-5)
tree    = colored(leaves, green) & colored(trunk, brown)
leaves  = sector(0, 180, 4)
trunk   = solidRectangle(1, 4)
~~~~~

What does `translated(..., 0, 0)` mean?  Well, it means don't move the picture at
all!  We wrote the `translated` there just to make things line up nicely.

### Rotation: Turning Your Pictures ###

You can *rotate* a picture to turn it, either clockwise or counter-clockwise.
To use `rotated`, you give it two things:

* A picture to rotate.
* A number of degrees to rotate the picture.  Negative numbers are clockwise,
  and positive numbers are counter-clockwise.

Here's an example:

~~~~~ . clickable
program = drawingOf(diamond)
diamond = rotated(square, 45)
square  = solidRectangle(4, 4)
~~~~~

A diamond is just a square, turned so it's diagonal.

### Scaling: Stretching Your Pictures ###

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

~~~~~ . clickable
program = drawingOf(oval)
oval    = scaled(base, 2, 0.5)
base    = solidCircle(4)
~~~~~

You should try to get a good feeling for the meaning of those scaling
factors.  Try changing the numbers in the example, and see if you can
guess what will happen before you press run.

Expressions
===========

Now that you've spent some time trying out pictures, let's learn a few
more tricks you can use.  Anything you can write after the equal sign
is called an *expression*.  For example:

* `circle(4)` is an expression.
* `colored(lettering("Help"), red)` is also an expression.
* `rectangle(1, 4) & circle(2)` is an expression.
* `leaves & trunk` is an expression.

However, `tree = leaves & trunk` is *not* an expression.  It's a
definition.  Can you tell the difference?  Expressions describe
something, but don't give it a name.  But every definition has an
expression inside, after the equal sign.  So expressions are pretty
important.

Nesting
-------

Remember how we used `rotated`?  Here's a quick reminder:

~~~~~ . clickable
program = drawingOf(diamond)
diamond = rotated(square, 45)
square  = rectangle(2, 2)
~~~~~

Nice!  However, naming everything like that can get tedious.  If you
have a simple shape, such as `rectangle(2, 2)`, you may not want
to bother giving it a name.  You can just describe the shape right where
the name would go.

Try it:

~~~~~ . clickable
program = drawingOf(diamond)
diamond = rotated(rectangle(2, 2), 45)
~~~~~

Or even:

~~~~~ . clickable
program = drawingOf(rotated(rectangle(2, 2), 45))
~~~~~

Careful, though!  You can avoid naming simple things, but if you
nest too much, you get parentheses inside of parentheses inside of
parentheses,  and pretty soon it's hard to tell what's going on!

You can also nest other things besides pictures.  Remember that `dark`,
`light`, and `transparent` were functions that modify colors.  But since
`dark(red)` and `light(green)` are colors themselves, so you can use the
same functions on them!  Check out some of these colors:

* `dark(dark(green))`
* `translucent(light(blue))`

Numbers
-------

Nesting can be used for numbers, too.  You can let the computer work out
math for you on numbers, too.  When you write math expressions, you can
use `+` and `-` the way you normally would.  To multiply, use `*`.  To
divide, use `/`.

Check out this code:

~~~~~ . clickable
program = drawingOf(design)
design  = rotated(rectangle(4, 0.2), 1 * 180 / 5)
        & rotated(rectangle(4, 0.2), 2 * 180 / 5)
        & rotated(rectangle(4, 0.2), 3 * 180 / 5)
        & rotated(rectangle(4, 0.2), 4 * 180 / 5)
        & rotated(rectangle(4, 0.2), 5 * 180 / 5)
~~~~~

We could have written `36`, '72', '108', '144', and `180` (the answers to
those math problems).  But this way, it's very clear what we are doing:
dividing 180 degrees into fifths, and then rotating a rectangle by each
amount.  And we don't have to worry about getting one of the answers
wrong!

Just like in math, you can use parentheses to group expressions, so
`3 * (6 - 2)` is `3 * 4`, which is `12`.

The Coordinate Plane
====================

To draw more precise shapes, we can use points on a "coordinate plane".  You
can see a coordinate plane right now, just by running this code:

~~~~~ . clickable
program = drawingOf(coordinatePlane)
~~~~~

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

Got it?  Great!

Points, Lines and Polygons
--------------------------

Now you can draw things like sequences of lines by giving a list of points in
the coordinate plane to a function called `polyline`:

~~~~~ . clickable
program = drawingOf(zigzag)
zigzag  = polyline([(-2, 0), (-1, 1), (0, -1), (1, 1), (2, 0)])
~~~~~

The square brackets form something called a *list*.  You haven't seen lists
before, but they are just what they sound like: collections of multiple
things in order.  You need to place your points in a list (in square brackets)
before giving them to the `polyline` function.

To draw a closed shape, use `polygon` instead.  Can you figure out the
mystery picture before you click Run?

~~~~~ . clickable
program = drawingOf(mystery)
mystery = polygon(
    [(-3, -4), (0, 5), (3, -4), (-4, 2), (4, 2), (-3, -4)])
~~~~~

If you prefer to fill in your shape, you can use `solidPolygon` instead of
`polygon` and you'll get a solid version:

~~~~~ . clickable
program = drawingOf(mystery)
mystery = solidPolygon(
    [(-3, -4), (0, 5), (3, -4), (-4, 2), (4, 2), (-3, -4)])
~~~~~

There are also `thickPolygon` and `thickPolyline` which use an extra
parameter for thickness:

~~~~~ . clickable
program = drawingOf(mystery)
mystery = thickPolygon(
    [(-3, -4), (0, 5), (3, -4), (-4, 2), (4, 2), (-3, -4)], 1)
~~~~~

Finally, if you'd like to connect points with a curved line instead of straight
line segments, there are functions called `curve` and `thickCurve` for curves
with endpoints, and `closedCurve`, `solidClosedCurve`, and `thickClosedCurve`
for curves that join back to their starting point in a loop.  These functions
work exactly like `polyline` and `polygon`, but just draw smooth curves instead.

Drawing with the Coordinate Plane
---------------------------------

A neat trick is to use the coordinate plane as you write your code.  Say
you want to draw a butterfly.  You might start by writing:

~~~~~ . clickable
program   = drawingOf(butterfly & coordinatePlane)
butterfly = blank
~~~~~

Now run your program, and you have a coordinate plane to measure what
points to use in your shapes.  When you're done, just remove the
`& coordinatePlane` to get rid of the guidelines.

Types
=====

We've seen many different kinds of things so far that show up in your
code: pictures, numbers, text, points, colors... maybe you're wondering how to keep them
all straight!  CodeWorld calls these kinds of things *types*.  You'll mostly see types in two
places:

* When you make a mistake, you'll often see types mentioned in *error*
  *messages* that tell you about the problem.
* If you want to, you can say things about types in your code.
  If you do, the computer then knows more about what you meant, and
  can sometimes explain the problems in your code better.

Simple Types
------------

Hear are some of the types that you've used in your code:

* `Program` is the type of the variable `program` that you define in all
  your code.
* `Picture` is the type for pictures.
* `Number` is the type for numbers.
* `Color` is the type for colors.
* `Text` is the type for pieces of text, usually written in quotes.

Notice that while variables start with a lower-case letter, types are
capitalized.

Type Annotations
----------------

To declare types in your code, you can use `::`, like this:

~~~~~
wheel :: Picture
wheel = solidCircle(size)

size :: Number
size = 4
~~~~~

You don't *have* to say what type things are.  It's completely optional,
and the computer can always figure that out on its own.  But if you do
say what your types are, two things happen:

* Other people reading your code can understand what's going on.
* When you make a mistake the computer can be more helpful explaining
  what's wrong.

List Types
----------

What about lists?  Would you guess their type is `List`?  Not quite!  There are
many types of lists: lists of numbers, lists of pictures, lists of colors, and
so on.  To write the type of a list, we first write the type of the things
*inside* the list, then surround it with square brackets.

~~~~~
numbers :: [Number]
numbers = [ 1, 2, 3, 4 ]
~~~~~

Points and Tuples
-----------------

What about a point, like the ones we used to make polylines and polygons?  It
actually works just fine to say the type is `Point`:

~~~~~
start :: Point
start = (0, 5)
~~~~~

When the computer talks about points, though, it sometimes calls their type
something different: `(Number, Number)`.  This is just a way to say what we
already know: a point is an ordered pair, with each part being a number!  It
turns out `Point` is just shorthand for `(Number, Number)`, and they both mean
the same thing.

Types like that, with parentheses and commas, are sometimes called *tuples*.  So
a `Point` is a specific kind of tuple.  Other tuples might use different types,
different numbers of things, and even different types for the different values
inside!

* `(Number, Color)` is a tuple type.  Some possible values are the pairs
  `(4, red)` or `(-3, dark(green))`.
* `(Number, Text, Number, Color)` is a tuple type with four values inside.  A
  possible value is `(3, "train", 10, blue)`.

Tuples will be used a lot more later on, when we're dealing with memory and
state.  For now, you'll mostly use them for points on the coordinate plane.

Functions
---------

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

In general, function types have arrows (`->`) in them.  On the left of the arrow
is the type of things that the function needs: its *domain*.  On the right side
of the arrow is the type of things that the function makes: its *range*.
