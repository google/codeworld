Animations are exciting, and get things moving.  But it's even better when your
programs interact with the person using them, like games other applications.  In
CodeWorld, programs that interact and respond to the person using them are
called *activities*, and you'll write them with a function called `activityOf`.

State
=====

The most important thing about an activity is its *state*, a word with just means
"the way things are right now".  The state of an activity is a *value* that
includes everything the computer needs to remember about how things are.

The idea of a program having memory is new.  You might have noticed that
animations have no memory at all: every new frame starts over, with only one
number: how many seconds since the start.  You have to be able to describe the
whole animation with a function that only sees that number of seconds!  When
your program is interacting with the user, though, what you see 17 seconds in
depends on not just the time, but also on what **happened** during that 17
seconds.  The state is where you keep the information you need the computer to
remember.

Parts of an Activity
====================

Drawings and animations were described by a single thing: for drawings, it was a
picture.  For animations, it was a function mapping numbers to pictures.  But an
activity is actually built out of *three* separate (but related) parts.  Here
they are:

Part 1: Initial state
---------------------

The first part of an activity is often called `initial`, and it tells you the
state when the activity starts.

Part 2: Change function
-----------------------

The second part of an activity is often called `step`, and it tells you how
the state changes when something happens.  That could be a click of a mouse, a
key press, or just time passing.

Part 3: Picture function
------------------------

The third part of an activity is often called `picture`, and it tells you how
the state should be drawn on the screen as a `Picture`.

Building Simulations
====================

It may sound complicated, but let's jump in and look at an example:

```
program                       = activityOf(initial, change, picture)
initial(rs)                   = (0, 0)
change(p, PointerPress(x, y)) = (x, y)
change(p, other)              = p
picture(x, y)                 = translated(rectangle(1, 1), x, y)
```

This example just shows a small square that moves when you click somewhere.  In
this case, the state - the way things are - is a point.  The `initial` function
tells you where the square starts, which is the point (0, 0) (sometimes called
the "origin").  The `change` function tells you how the state changes when
things happen: it moves when you click somewhere, and otherwise stays the same.
And the `picture` function tells you what the program looks like: it looks like
a small square, in the location you last clicked.

Sometimes, an activity doesn't change with mouse clicks, but just to the normal
passing of time.  Consider this one:

```
program                         = activityOf(initial, change, picture)
initial(rs)                     = (5, 0)
change((x, y), TimePassing(dt)) = (x - y * dt, y + x * dt)
change(p, other)                = p
picture(x, y)                   = translated(rectangle(1, 1), x, y)
```

In this case, the state is still a point: the location of an object, but it
starts to one side.  The x and y coordinates change over time, by amounts that
depend on where the object is now:

* When the object is near the top of the screen, so `y` is a positive number,
  it's pushed to the left.
* When it's near the bottom and `y` is negative, it's pushed to the right.
* When it's near the right, so `x` is positive, it's pushed up.
* When `x` is negative on the left side, it's pushed down.

Can you guess what this will look like?  Try it and find out!

Activities can be a lot like science experiments.  You get to describe the
**rules** for how things change, but the overall behavior of the system can
still surprise you!

Choosing a State
----------------

The state is the first big choice you make when building an activity.  In the
activities above, the state was a point - an ordered pair with an x and y
coordinate.  When you write your own simulations, though, the state can be
whatever type you choose.  The state is your answer to this question:  What do
I need to *remember* for the activity to continue?

Think of these possibilities:
* Do you need to remember the *locations* of things?
* Do you need to remember the *speed* things are moving?
* Do you need to remember the *direction* things are moving?
* Do you need to remember the *angle* of something that turns?

Anything you need to remember will go into your state type.

Reacting to Events
------------------

Once you've chosen a state, the next big choice you make is how the state
should react to things happening.  The things that can happen are values of a
type called `Event`.  There are several types of events:

* `KeyPress` and `KeyRelease` tell you that the person using your program has
  pressed or released a key on the keyboard.
* `PointerPress` and `PointerRelease` tell you that they have clicked or let
  go of a mouse or a touch screen.
* `PointerMovement` tells you they have moved the location they are pointing
  to with the mouse or touch screen.
* `TimePassing` tells you that some time has passed.  These events are
  constantly happening.

To respond to some kind of event, you write an *equation* of your `change`
function, which describes what happens when that kind of event is received.
So your `change` function is normally defined with several equations using
*pattern matching*.

One mistake that's easy to make is forgetting to say what happens with an
event that you *don't* care about.  You should always end your `change`
function with an equation like

```
change(state, anything) = state
```

That equation matches any event that hasn't been matched yet, and says that
if some other event happens, the new state should be the same as the old
state.

**From here on, the guide hasn't been updated.**

### States with one number ###

The simplest kind of state is a single number.  Let's build a simulation to move
a box across the screen.  You could have done this with an animation, but this
makes a good starting point to learn how things work with simulations.

```
program     = simulationOf(initial, step, picture)
initial(rs) = -10
step(x, dt) = x + dt
picture(x)  = translated(solidRectangle(1, 1), x, 0)
```

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

### Simulations with multiple numbers ###

Adding a second number to the state lets you describe different kinds of change
at the same time.  Let's try to make a rolling wheel, which will need to move
(translation) and turn (rotation) at the same time.  In the state, we'll need
both an x coordinate, and an angle of rotation.

```
program = simulationOf(initial, step, picture)
initial(rs) = (-10, 0)
step((x, angle), dt) = (x + dt, angle - 60 * dt)
picture(x, angle) = translated(rotated(wheel, angle), x, 0)
wheel = circle(1) & solidRectangle(2, 1/4) & solidRectangle(1/4, 2)
```

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

```
program = simulationOf(initial, step, picture)
initial(rs) = (-9, -9, 15)
step((x, y, vy), dt) = (x + 6 * dt, y + vy * dt, vy - 10 * dt)
picture(x, y, vy) = translated(solidCircle(1/2), x, y)
```

As you can see, the velocity in the y direction, `vy`, starts at 15 units per
second upward.  But as time passes, it decreases, until eventually it becomes
negative, and the ball falls back down again.

* What would happen if the ball started at a different position?
* How would you change the effect of gravity to simulate a ball on the moon?

Simulating Physics
==================

One of the most common things to do with simulations is simulate physics.
To do this, we'll want to make some observations about the physical world.

Position and Velocity
---------------------

The first of these observations was made by Isaac Newton, who first described
many of the foundations of physics as we understand them today.

> *Newton's First Law of Motion*: Every object persists in its state of rest
> or uniform motion in a straight line unless it is compelled to change that
> state by forces impressed on it.

Another way of putting this is that the *state* of an object consists of both
its position, and its velocity.  You can make this observation yourself: when
you throw a ball, your hand is pushing the ball; but when you let go, you are
no longer pushing the ball forward.  Does it stop right away?  No!  The ball
keeps its current velocity, and keeps on moving until something else happens
to stop it.

What this means is that any time you have an object moving with physics in a
simulation, you should start out thinking about both *position* and *velocity*
of that object as part of the state.

The last example above already did for this for movement in the y direction.
But we could rewrite it to keep track of velocity in the x direction, as well.

```
program = simulationOf(initial, step, picture)
initial(rs) = (-9, -9, 6, 15)
step((x, y, vx, vy), dt) = (x + vx * dt, y + vy * dt, vx, vy - 10 * dt)
picture(x, y, vx, vy) = translated(solidCircle(1/2), x, y)
```

This simulation does the same thing as the one before it, but notice how we've
taken the constant `6`, and moved it from the code for the `step` function to a
part of the state.  That is an acknowledgement that although the velocity in x
is always 6 at the moment, there's no reason it couldn't be something different!

Gravity
-------

This section is still in progress.

Other Forces
------------

This section is still in progress.

Designing With Simulation
=========================

This section is still in progress.

Horizontal Composition
----------------------

This section is still in progress.

Vertical Composition
--------------------

This section is still in progress.
