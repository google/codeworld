Now that you've built interactive programs, it's time to try something new.  You
can use your computational skills to explore the way that 

What is Modeling?
=================

Modeling refers to using mathematical structures, such as functions and
equations, to represent a real-world process or system.  Models are used to
explore lots of systems using mathematical rules.  Scientists use models to
predict the effects of different weather patterns.  Engineers use modeling to
decide how well a plane will fly.  Banks use models to make choices in how to
invest, and predict how much money they can make from different investments.

In this section, we'll explore using your CodeWorld skills for various modeling
tasks.

Randomness
==========

Many computational models involve the use of random numbers.  In games, you can
use random numbers to shuffle cards, roll dice, pick monsters, and much more.
Randomness can also be used in computational models.

You've already used functions like `shuffled` and `randomNumbers` to pick
numbers in an unpredictable way.  Each time your program runs, though, you'll
get exactly the same result.  That's because even though when these functions
are hard to predict, they are still just complicated math calculations!  If
you provide the same seed, you'll always get the same answer.

To get different answers each time, you need a way to get a number from outside
your program.  The way to do this is with the argument to your `initial`
function in an *activity*.

The parameter to `initial` is a list of random numbers.  Just like the result
of the `randomNumbers` function, the list goes on forever, and each number is
between 0 and 1.  But unlike the `randomNumbers` function, these numbers are
different every time the program starts.

Choosing Random State
---------------------

Let's start with an activity that moves a ball across on the screen.

~~~~~ . clickable
program = activityOf(initial, change, picture)

initial(rs) = (-8, 2)

change((x, vx), TimePassing(dt)) = (x + vx * dt, vx)
change((x, vx), other)           = (x, vx)

picture(x, vx) = translated(circle(1), x, 0)
~~~~~

Suppose you wanted the speed of the ball to be different each time.  The easiest
way to do this is to use the first random number sent to `initial` as the speed.

~~~~~
initial(rs) = (-8, rs # 1)
~~~~~

A speed between 0 and 1 unit per second is pretty slow, though.  You can scale
the number into a range, by multiplying by the size of the range, and adding the
lower bound.  So to choose a range of speeds between 1 and 5, you'd write
`4 * rs # 1 + 1`.  Here is that complete code.

~~~~~
initial(rs) = (-8, vx)
  where vx = 4 * rs # 1 + 1
~~~~~

Because your `initial` function receives an infinite list of random numbers, you
can use as many of them as you need for different quantities that you wish to
choose at random.  Here, for example, is what it looks like to choose both the
starting position and speed at random.  Notice that each variable uses a different
random number from the sequence.

~~~~~
initial(rs) = (x, vx)
  where x   = 5 * rs # 1 - 5
        vx  = 4 * rs # 2 + 1
~~~~~

Saving Randomness
-----------------

The example above shows you how to choose a random *initial* state for your
program.  But what if you want random things to continue occurring as the
program continues?  You'll need to either describe all the random events that
should occur in your activity when describing the initial state, or hold on to
the random numbers given to `initial` as *part* of the state.

Here's an example of the first strategy.  The square will turn a different random
color every time the mouse is clicked.  Notice that the state contains not just
this color, but the entire future sequence of colors!  The `initial` function
uses a list comprehension to define an entire infinite sequence of colors.
The `change` and `picture` functions use pattern matching with `:` to separate
the current color at the beginning of the list from the list of future colors
that come later.

~~~~~ . clickable
program = activityOf(initial, change, picture)

initial(rs) = [ HSL(360 * r, 0.75, 0.5) | r <- rs ]

change(c:cs, PointerPress(p)) = cs
change(c:cs, other)           = c:cs

picture(c:cs) = colored(solidRectangle(10, 10), c)
~~~~~

You might wonder how the computer ever finishes *calculating* an infinite list
of colors, all before the program even starts!  Don't worry.  The computer is
good at knowing when it needs to finish doing a calculation, and it just figures
out the colors as it goes.

Another way to write this same program would be to remember the list of random
numbers you get at the start, and compute the color when drawing the picture
itself.  The program is the same, but the code looks a little different:

~~~~~ . clickable
program = activityOf(initial, change, picture)

initial(rs) = rs

change(h:hs, PointerPress(p)) = hs
change(h:hs, other)           = h:hs

picture(h:hs) =
    colored(solidRectangle(10, 10), HSL(360 * h, 0.75, 0.5))
~~~~~

You can write your code either way.  However, as a rule of thumb, the more
meaningful your state is, the easier it is to understand your code.  A list
of colors is more meaningful than a list of plain numbers, so most
programmers would prefer the first program here.  It's good to understand both
ideas, though, because there are times when remembering the original random
list is more convenient, too.

Applying Random Numbers
-----------------------

Once you have the random numbers to work with, there are a few tricks you can
use to get the effect you want from them.

### Rescaling random numbers ###

This section is still in progress.

### Making a decision based on probability ###

This section is still in progress.

### Timing random events based on frequency ###

This section is still in progress.

Decomposition with Random Numbers
---------------------------------

This section is still in progress.
