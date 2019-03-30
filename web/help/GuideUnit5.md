Animations are exciting, and get things moving.  But it's even better when your
programs interact with the person using them, like games and other applications.
In CodeWorld, programs that interact and respond to the person using them are
called *activities*, and you'll write them with a function called `activityOf`.

State
=====

The most important thing about an activity is its **state**, a word with just
means "the way things are right now".  We can talk about the state of lots of
things!  The state of a light switch is whether it's on or off.  The state of
a game of tic tac toe includes which marks (X or O) are in which squares, and
whose turn it is.  There are even states for much more complex things: you
might have heard that the President of the United States gives a speech every
year called the State of the Union, and that speech is about the *state*
(that is, the way things are right now) in the United States.

For an activity built with the `activityOf` function, the state is a *value*
that includes everything the computer needs to *remember* about what's going on.
This idea of a program remembering things is new.  You might have noticed that
animations have no memory at all: every new frame is drawn knowing only how
many seconds have gone by since the start of the program.  To build an
animation, you needed to describe the whole animation with a function that
computes everything from that number of seconds!  When your program is a game
or interactive activity, though, what you see after 17 seconds depends on not
just the time, but also on what **happened** during that 17 seconds.  The
state is where you keep the information you need the computer to remember.

Parts of an activity
====================

Drawings and animations were described by a single thing: for drawings, it was a
picture.  For animations, it was a function mapping numbers to pictures.  But an
activity is actually built out of *three* separate (but related) parts.  Here
they are:

1. Initial state.  The first part of an activity is often called `initial`, and
   it tells you the state when the activity starts.
2. Change function.  The second part of an activity is often called
   `change`, and it tells you how the state changes when something happens.
   That could be a click of a mouse, a key press, or just time passing.
3. Picture function.  The third part of an activity is often called `picture`,
   and it tells you how the state should be drawn on the screen as a `Picture`.

Numbers as state
----------------

In a very simple activity, your state might be a single number.

~~~~~ . clickable
program                  = activityOf(initial, change, picture)
initial(rs)              = 0
change(x, KeyPress(" ")) = x + 1
change(x, other)         = x
picture(x)               = translated(solidCircle(1/2), x, 0)
~~~~~

Take a moment to dissect this program, and understand what each line means.

~~~~~
program = activityOf(initial, change, picture)
~~~~~

This line defines the `program` to be an activity.  As mentioned earlier,
an activity needs to provide three functions to answer questions about
the state: what is the *initial* state at the beginning of the program,
how does it *change*, and what *picture* should be drawn from it?  The
answer to each question has a name, and will be defined later.

~~~~~
initial(rs) = 0
~~~~~

The `initial` function causes the state to begin at zero.  In this
activity, the state keeps track of the x coordinate of a dot.  The dot
begins with an x coordinate of zero, in the middle of the screen.

Don't worry about the `rs` argument right now.  It must be there, but
you can ignore it until you learn to use it later.

~~~~~
change(x, KeyPress(" ")) = x + 1
~~~~~

This is the first of two equations for the `change` function.  It says
that when the space bar is pressed, the new state (that is, the new x
coordinate of the dot) should be one greater than the old state.  This
causes the dot to move one unit to the right.

There are different types of events that you can handle, and `KeyPress`
is one of them.  You'll see more of them as you explore activities
further later in this section.

~~~~~
change(x, other) = x
~~~~~

This is the second of two equations for the `change` function.  It
says that when anything else happens that wasn't already handled in
earlier equations, the state should not change.  You'll always need
this equation as the last one in the definition of `change`.
Otherwise, your program will crash any time an event happens that
you didn't plan for.

~~~~~
picture(x) = translated(solidCircle(1/2), x, 0)
~~~~~

Finally, this equation explains what the screen should look like.  The
state (the x coordinate) can change, but the rest of the picture looks
the same.

Tuples as state
---------------

While a few simple activities can be built with a single number for state,
most of the time you'll want to keep track of more than one value.  You
do this with tuples.  An ordered pair (such as the `Point`s you've used
in the past) is a tuple with two values in it, but tuples can have as
many values as you like.

This activity has both an x and y coordinate as state.

~~~~~ . clickable
program                           = activityOf(initial, change, picture)
initial(rs)                       = (0, 0)
change(point, PointerPress(x, y)) = (x, y)
change(point, other)              = p
picture(x, y)                     = translated(rectangle(1, 1), x, y)
~~~~~

Here, the small square moves when you click anywhere.  The state - what
the computer should remember - is the point where the square should
appear.  The `initial` function tells the computer where the square
starts, which is the point (0, 0) (sometimes called the "origin").  The
`change` function tells the computer how the state changes when things
happen: it moves when you click somewhere, and otherwise it stays the same.
And the `picture` function tells the computer what the program looks like:
a small square, in the correct location.

Continuous changes
==================

Sometimes, an activity doesn't change with mouse clicks, but just the normal
passing of time.  Consider this one:

~~~~~ . clickable
program                         = activityOf(initial, change, picture)
initial(rs)                     = (5, 0)
change((x, y), TimePassing(dt)) = (x - y * dt, y + x * dt)
change(p, other)                = p
picture(x, y)                   = translated(rectangle(1, 1), x, y)
~~~~~

The new form of event, called `TimePassing`, is used to describe how the
state changes smoothly or continuously over time.  The argument, `dt`,
is short for "difference in time", and it describes how much time has
elapsed since *the last frame*.  Notice how this is different from
animations, where `t` always referred to the *total* time that had passed
throughout the program.  While `t` grew larger with each frame, `dt` is
always a small number, usually somewhere around 1/60.  It is still a time,
though, and you can multiply it by a rate of change to calculate the amount
of change, which can then be added to the state to produce continuous
movement.

In this example, the state is still the location of an object.  This object
starts to one side, and the x and y coordinates change over time by amounts
that depend on where the object is now.  

* When the object is near the top of the screen, the `y` coordinate is a
  positive number, so subtracting `y * dt` moves the object to the left.
* When it's near the bottom, `y` is negative, so subtracting `y * dt` moves
  it to the right.
* When it's near the right, `x` is positive, so adding `x * dt` moves it up.
* When it's near the left, `x` is negative, so adding `x * dt` moves it down.

Can you guess what this will look like?  Try it and find out!

As you might have noticed in this example, activities can be a lot like
science experiments.  You get to describe the **rules** for how things
change, but the overall behavior of the system can still surprise you!
You're running an experiment to see what the consequences of those rules
will be.

Invisible state
---------------

So far, you've seen activities where the state is visible from the
picture.  After all, what would be the point of state that you can't see?
Actually, invisible state is very useful.  Even if you can't see it right
now, it can affect how the rest of the state will change in the future.

A simple example is a timer.  Although in any one instant, you cannot see
how long remains until the timer finishes, the timer can still be important
because it triggers more changes later.  The next example defines an
activity where a ball begins at the top of the screen, and move one unit
every second toward the bottom.  The activity needs *two* numbers as
state: a current position, and then the time until the next move.

~~~~~ . clickable
program           = activityOf(initial, change, picture)
initial(rs)       = (9, 3)
change((y, timer), TimePassing(dt))
  | timer < 0     = (y - 1, 3)
  | otherwise     = (y, timer - dt)
change(p, other)  = p
picture(y, timer) = translated(solidCircle(1), 0, y)
~~~~~

The `change` function's equation for `TimePassing` has two cases here:
if there is no more time left, then the object is moved down one unit.
Otherwise, the object is not moved, but the elapsed time is subtracted
from the counter so that it accurately tracks the time left.  The timer
isn't mentioned at all in `picture`, because it cannot be seen in a
single frame; but its effect can certainly be seen over time.

Reacting to Events
==================

You've already seen examples of several events that can cause changes
in an activity's state.  A more complete list of events is here.

* `KeyPress` and `KeyRelease` tell you that someone has pressed or released
  a key on their keyboard.  The argument is a text name for the key.
* `PointerPress` and `PointerRelease` tell you that someone has clicked or
  let go of a mouse button, touch screen, or other pointer device.
* `PointerMovement` tells you someone has moved the location they are
  pointing to with the mouse or touch screen.
* `TimePassing` tells you that some time has passed, so that you can
  update things that change continuously over time.

To respond to some kind of event, you write an *equation* of your `change`
function, which describes what happens when that kind of event is received.
So your `change` function is normally defined with several equations using
*pattern matching*.

Remember to say what happens with an event that you *don't* care about.
You should always end your `change` function with an equation like

~~~~~
change(state, other) = state
~~~~~

That equation matches any event that hasn't been matched yet, and says that
if some other event happens, the new state should be the same as the old
state.

Simulating Physics
==================

One of the most common things to do with activities is simulate something
moving, falling, bouncing, or otherwise reacting to the world around it.
The rules that govern physical objects moving and bumping each other are
studied as part of *physics*.  To get started with this, we'll want to make
some observations about the physical world.

Position and Velocity
---------------------

The first of these observations was made by Isaac Newton, who first described
many of the foundations of physics as we understand them today.

> *Newton's First Law of Motion*: Every object persists in its state of rest
> or uniform motion in a straight line unless it is compelled to change that
> state by forces impressed on it.

Another way of putting this is that the *state* of a physical object consists
of both its position and its velocity.  You can make this observation
yourself: when you throw a ball, your hand is pushing the ball; but when you
let go, you are no longer pushing the ball forward.  Does it stop right away?
No!  The ball keeps its current velocity, and keeps on moving until something
else happens to stop it.

What this means is that any time you have an object moving with physics in an
activity, you should start out thinking about both *position* and *velocity*
of that object as part of the state.  Here's an example with an object that
remembers its position and velocity, in both the x and y directions.

~~~~~ . clickable
program = activityOf(initial, change, picture)
initial(rs) = (-9, -9, 6, 15)
change((x, y, vx, vy), TimePassing(dt)) =
    (x + vx * dt, y + vy * dt, vx, vy - 10 * dt)
change(p, other) = p
picture(x, y, vx, vy) = translated(solidCircle(1/2), x, y)
~~~~~

This object will continue moving as Newton's law guarantees: in a straight line
at a constant speed, since nothing is stopping it.  As Newton suggests, the
velocity is part of the state of the object.

Gravity
-------

This section is still in progress.

Other Forces
------------

This section is still in progress.

Designing With State
====================

This section is still in progress.

Horizontal Composition
----------------------

This section is still in progress.

Vertical Composition
--------------------

This section is still in progress.
