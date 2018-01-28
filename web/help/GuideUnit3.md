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

    program      = animationOf(propellor)
    propellor(t) = rotated(solidRectangle(10, 1), 60 * t)

See if you can explain to yourself or someone else what is happening here.

### Analyzing the animation ###

The first line asks for an animation, by using the `animationOf` function.  The
parameter is a *function* that can be used to produce the frames of the
animation.  In our example, this function is called `propellor`.

The second line defines the `propellor` function.  The argument to this function,
which we will usually call `t`, stands for the time in seconds since the program
was started.  The result of the function should be a picture, which is the frame
to display at that time.  Unlike movies and television shows, which only have a
fixed sequence of frames, computers can work as fast as possible to draw as many
frames as they can, by following the same pattern.

> (Have you ever heard of the graphics in video games described in "frames per
> second"?  This is, literally, the number of different frames the computer is
> capable of drawing in one second.  Slower computers can't draw as many frames,
> which can make motion appear jerky and uneven.  Faster computers can draw more
> frames, which makes the motion appear smooth and natural.)

We write the *result* of the `propellor` function as `propellor(t)`, and it's
just an ordinary picture - one frame of the animation.  But what exactly this
frame looks like depends on `t`.  Instead of a specific angle of rotation, an
expression `60 * t` is used.  Just as with any function, this is evaluated by
substitution.  So the frame that is drawn 4.5 seconds in is rotated by an angle
of `60 * 4.5`, which is 270 degrees.

It might help to make a table, like this:

| Time (`t`)  | Frame (`propellor(t)`)               |
| ----------- | ------------------------------------ |
|    `0.0`    | `rotated(solidRectangle(10, 1),  0)` |
|    `0.1`    | `rotated(solidRectangle(10, 1),  6)` |
|    `0.2`    | `rotated(solidRectangle(10, 1), 12)` |
|    `0.3`    | `rotated(solidRectangle(10, 1), 18)` |
|    `0.4`    | `rotated(solidRectangle(10, 1), 24)` |
|    `0.5`    | `rotated(solidRectangle(10, 1), 30)` |

See how each frame draws a picture that's similar to the previous one, but just
a few degrees different in rotation?  It makes a flipbook, but in one easy
expression.

How fast (in degrees per second) is the propellor rotating?

### Continuity ###

The program above shows a propellor that appears to be moving.  But not all
animations look like motion!  Try this one:

    program = animationOf(f)
    f(t) = translated(solidCircle(1), x, 0)
      where x = 10 * randomNumbers(t) # 1

Why did the propellor look fine, but this animation was flickering more than
moving?  The problem is that `f` here is not *continuous*.  When a function is
continuous, that means that if its inputs are close enough, the outputs should
be close, as well.  In other words:

* You don't want your animation function to always give exactly the *same*
  picture. The animation wouldn't move.
* But you do want your animation function to give *similar* pictures.  It's the
  small, slight changes from one frame to the next that create a smooth
  animation.

Lucky for you, most of the simple math expressions you can write with `t` in
them are continuous!  But keep this in mind, because in more complex animations
later, you'll have to think harder to make sure your animation is continuous.

Kinds of motion
---------------

Animations can be built from many kinds of motion.  Anywhere you have used
a number in the description of a picture, you could achieve change over time
by using an expression in terms of `t` instead!

Here are a few possibilities:

* The radius of a circle.
* The width or height of a rectangle.
* The amount by which a shape is translated, scaled, or rotated.
* The angles of an arc or sector.
* The x or y coordinates of points in a polyline or polygon.
* RGB or HSL values in a color.

The list goes on and on!  And you don't need to settle for just one of these.
You can use `t` as many times in your animation as you like!  This example
combines change in rotation and translation at the same time:

    program  = animationOf(wheel)
    wheel(t) = translated(rotated(tire, -60 * t), t - 10, y)
    tire     = circle(1) & solidRectangle(0.1, 2)

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
need to give a pattern that can describe any of its frames.

When you define an animation `ball` by talking about a single frame
`ball(t)`, your definition is establishing a pattern.  Now you can find the
specific frame `ball(0)` by substituting `0` for `t`.  You can find the
specific frame `ball(1.735)` by substituting `1.735` for `t`.  And so on.
Having a name, `t`, that stands for the current time is a tool you use to
describe the pattern.

We'll refer back to this as we explore some examples of top-down design
with animation.

### Combining animations with `&` ###

The `&` operator is used to combine pictures.  But what if you have two
animations and want to show them at the same time?  You can't use `&` to
combine the two animations.  So this won't work:

    program = animationOf(a & b)
    a(t)    = rotated(solidRectangle(1, 1), 45 * t)
    b(t)    = circle(t)

The problem is that `a` and `b` are animations rather than pictures, so
they can't be combined using `&`.  If you try, you'll see an error message
complaining that the types don't match.  The `&` needs pictures to combine,
but you've given it animations (which are functions), instead.

But you can use `&` to combine two *pictures*, in the pattern that
describes one frame of the animation.  This will work:

    program = animationOf(c)
    c(t)    = a(t) & b(t)
    a(t)    = rotated(solidRectangle(1, 1), 45 * t)
    b(t)    = circle(t)

We've instead defined a new animation, `c`, and established the pattern
that each frame of `c` is obtained by combining the frames from `a` and
`b` at that moment in time.

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

### Inverse: Approaching a point ###

When you want a function that keeps getting closer to a point, but doesn't ever
quite get there, the inverse function is your tool of choice.  Here's an
example:

    program = animationOf(scene)
    scene(t) = translated(solidCircle(1/4), 0, 5 - 3 / (t + 1))

The two questions to ask are:

* What *limiting value* should the motion approach (but never reach)?
* At what *starting offset* from the limiting value should the motion begin?

The expression is then `limit + start / (t + 1)`.  Notice that we divide by
`t + 1` instead of `t`?  That's to avoid division by zero!  Time is never
negative, but it can and will be zero.

Piecewise motion
----------------

Still more motion follows different patterns at different times.  When there
are several distinct steps to the change in an animation, we call it "piecewise"
because it have distinct pieces, which are different from each other.

You can describe piecewise motion in your programs using functions like `min` and
`max` and `remainder`, or by using conditionals, like guards.

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
90, it jumps back to zero.  The result is a motion that increases for 90
degrees, then falls back down.

### Guards ###

The way to define a piecewise function is to use *guards*.  You saw guards in
the previous part.  Guards are used to give several different definitions for
an entire function, each attached to some condition that says when it applies.

Here's an example of an animation using guards.

    program       = animationOf(flight)

    flight(t)
      | t <  2    = translated(rocket, -5, 2 * t)
      | t <  5    = translated(rotated(rocket, 60 - 30 * t), -5, 4)
      | t < 10    = translated(rotated(rocket, -90), 2 * t - 15, 4)
      | t < 13    = translated(rotated(rocket, 15 * t - 240), 5, 4)
      | otherwise = translated(rotated(rocket, -45), 2 * t - 21, 2 * t - 22)

    rocket = solidRectangle(1, 4)

### The multi-step animation pattern ###

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

Graphing Functions
==================

We've now seen a lot of examples of functions that can be used to get
specific effects in animation: linear, periodic, quadratic, min/max,
remainder, and more!  How can you keep these straight?

There's a convenient visual way to understand what a function does, called a
graph.  Graphs can only be used for functions between *numbers*, not colors
or pictures.  But when you do have a function between numbers, the graph is
an easy way to understand the function at a glance.

To draw the graph of a function, first draw a horizontal axis to represent `t`,
the time in seconds.  Then for a lot of possible values of `t`, find the result
of the function on the vertical axis.  Plot those points on the coordinate plane
and connect them with a smooth line.

You can practice graphing on paper, but it's also not too hard to draw the
graph of a function using CodeWorld.  Here's a program that draws the graph of a
sine (periodic) function.

    program = drawingOf(graph & coordinatePlane)
    graph = curve([ (x, f(x)) | x <- [0, 0.1 .. 10] ])

    f(t) = 3 * sin(90 * t)

See how the back-and-forth pattern of the sine function is visible on the
screen?  You can easily see that this function will start at 0, and as time
increases, it will change up to 3, then down to -3, then back up again, and so
on.  (The graphs we are drawing here start 0 on the horizontal axis, because the
input to an animation is never zero.)

You can replace the definition of `f` by any other function to see its graph
instead.  Try some of these, and see if you can describe to yourself where that
function starts and how it changes over time:

* `f(t) = 3 * t - 7`
* `f(t) = 5 - 4 / (t + 1)`
* `f(t) = remainder(t, 2)`
* `f(t) = quotient(t, 2)`

Choosing Functions By Graph
---------------------------

You should take the time to familiarize yourself with the graphs of different
functions.  When you want a certain pattern of change, one way to start is to
first sketch a rough graph of the function you want, and then work backwards to
identify the type of expression.

* If the graph is a straight line, you want a linear expression:
  `speed * t + start`
* If the graph follows a wave, you want a periodic expression:
  `amplitude * sin(frequency * t + phase) + rest`
* If the graph follows a constant arc like the path of a baseball hit high into
  the air, then you want a quadratic expression:
  `accel / 2 * t^2 + speed * t + start`.

and so on.  Remembering this table will be very useful to you.  Happy animating!
