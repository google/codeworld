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
are  hard to predict, they are still just complicated math calculations!  If
you provide the same seed, you'll always get the same answer.

To get different answers each time, you need a way to get a number from outside
your program.  The way to do this is with the argument to your `initial`
function.  This argument only exists in simulations, interactions, and
collaborations - so drawings and animations cannot give different results each
time.

The parameter to `initial` is a list of random numbers.  Just like the result
of the `randomNumbers` function, the list goes on forever, and each number is
between 0 and 1.  But unlike the `randomNumbers` function, these numbers are
different every time the program starts.

Choosing Random State
---------------------

Let's start with a simulation that moves a circle across the screen.

~~~~~
program = simulationOf(initial, step, picture)

initial(rs)       = (-8, 4)
step((x, vx), dt) = (x + vx * dt, vx)
picture(x, vx)    = translated(circle(1), x, 0)
~~~~~

Suppose you wanted the speed of the ball to be different each time.  The easiest
way to do this is to use the first random number sent to `initial` as the speed.

~~~~~
initial(rs)       = (-8, rs # 1)
~~~~~

A speed between 0 and 1 is pretty slow, though.  You can scale the number into a
range, by multiplying by the size of the range, and adding the lower bound.  So
to choose a range of speeds between 1 and 5, you'd write `4 * rs # 1 + 1`.  Here
is that complete code.

~~~~~
initial(rs)       = (-8, 4 * rs # 1 + 1)
~~~~~

Of course, you can also use pattern matching with the `:` operator to get
numbers from the list.  This is particularly convenient when you want to choose
more than one quantity at random.  Here, for example, is what it looks like to
choose both the starting position and speed at random.

~~~~~
initial(x:vx:rs)       = (5 * x - 5, 4 * vx + 1)
~~~~~

Saving Randomness
-----------------

The example above shows you how to choose a random *initial* state for your
program.  But what if you want random things to continue occurring as the
program continues?  You'll need to either describe all the random events that
should occur in your simulation when describing the initial state, or hold on to
the random numbers given to initial as *part* of the state.

Here's an example of the first strategy.  The square will turn a sequence of
random colors.  The code describes the entire infinite sequence of colors using
a list comprehension in the state.

~~~~~
program = simulationOf(initial, step, picture)

initial(rs) = (0, [ HSL(360 * r, 0.75, 0.5) | r <- rs ])

step((t, c:cs), dt) | t > 1     = (0, cs)
                    | otherwise = (t + dt, c:cs)

picture(t, c:cs) = colored(solidRectangle(10, 10), c)
~~~~~

You might wonder how the computer ever finishes *calculating* an infinite list
of colors, all before the simulation even starts!  Don't worry.  The computer is
good at knowing when it needs to finish doing a calculation, and it just figures
out the colors as it goes.

Another way to write this same program would be to just remember the starting
list of random numbers, and compute the colors from that when drawing the
picture itself.  The program is the same, but the code looks a little different:

~~~~~
program = simulationOf(initial, step, picture)

initial(rs) = (0, rs)

step((t, h:hs), dt) | t > 1     = (0, hs)
                    | otherwise = (t + dt, h:hs)

picture(t, h:hs) =
    colored(solidRectangle(10, 10), HSL(360 * h, 0.75, 0.5))
~~~~~

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
