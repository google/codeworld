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

We now want to describe programs that have *state*, or memory.  In CodeWorld,
the simplest programs with state are called simulations.

Parts of a Simulation
=====================

Pictures and animations were described by a single thing: for a pictures, it was
a picture.  For animations, it was a function mapping numbers to pictures.  But
a simulation is actually built out of *three* separate (but related) parts.
Here they are:

Part 1: Initial state
---------------------

The first part of a simulation is often called `initial`, and it tells you how
the simulation is when it starts.

Part 2: Step function
---------------------

The next part of a simulation is often called `step`, and it tells you how the
simulation changes when time passes.

Part 3: Picture function
------------------------

The final part of a simulation is often called `picture`, and it tells you how the
simulation should be presented on the screen as a `Picture`.

All three of these parts share some data called the "world", which records
everything you want to *remember* about the simulation as it happens.

Building Simulations
====================

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

### Simulations with one number ###

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

### Simulations with multiple numbers ###

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
    step((x, y, vy), dt) = (x + 6 * dt, y + vy * dt, vx, vy - 10 * dt)
    picture(x, y, vy) = translated(solidCircle(1/2), x, y)

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

    program = simulationOf(initial, step, picture)
    initial(rs) = (-9, -9, 6, 15)
    step((x, y, vx, vy), dt) = (x + vx * dt, y + vy * dt, vx, vy - 10 * dt)
    picture(x, y, vx, vy) = translated(solidCircle(1/2), x, y)

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
