Interactions
============

We've learned to do a lot of cool things with our programs by now, but there's
one thing missing: interaction! We can't make games if people can't interact
with our programs. We can create programs that people can interact with by using
the `interactionOf` function in place of the `simulationOf` function.
Interactions allow you to decide what will happen when someone presses a key or
clicks the mouse while your program is running.

Interactions have almost the same form as simulations. The `initial`, `step`,
and `picture` parts of an interaction mean exactly the same thing.  The only
difference is that interactions have an extra argument: a function that is
commonly called `event`.  The `event` function fits into `interactionOf` between
the `step` and `picture` functions, so while the first line of a simulation
looks like this:

    program          = simulationOf(initial, step, picture)

the first line of an interaction looks like this:

    program          = interactionOf(initial, step, event, picture)

Here's how the `event` function of an interaction fits in with the parts we are
familiar with from simulations.

User Interface Events
=====================

To get any further, you'll need to understand user interface events.

The `event` Function
--------------------

As we have already said, you can use the `event` function to tell the computer
how your program's state should change when someone uses the keyboard or mouse.
In simulations, the state stayed the same.  You can define `event` to say this
explicitly:

    program     = interactionOf(initial, step, event, picture)
    initial(rs) = -10
    step(x, dt) = x + dt
    event(x, e) = x
    picture(x)  = translated(solidCircle(1/2), x, 0)

This `event` function produces the same state as before, regardless of any event
that occurred.  In other words, this same program could have been described with
a simulation instead.  It is not a very interesting interaction!  To make a more
interesting program, you will need to know how to handle different types of
events. 

We haven't seen events yet, so let's take a to look at the types of events you
might deal with.

The `Event` Type
----------------

`Event` is its own *type*, just like `Picture` and `Number` and `Color`.  Values
of this type represent ways the computer can interact with their computer, such
as pressing or releasing a key or the mouse.  Each kind of event is a
*constructor* for the `Event` type.  Here are the constructors you can look for:

* KeyPress :: Text -> Event
* KeyRelease :: Text -> Event
* PointerPress :: Point -> Event
* PointerRelease :: Point -> Event
* PointerMovement :: Point -> Event

Key Events
----------

The first two event constructors, `KeyPress` and `KeyRelease`, deal with what
happens when someone presses a key.  `KeyPress` indicates that someone pressed a
key, while `KeyRelease` indicates that they released it.  These are slightly
different, so it's important to consider which one you want.  In a fast-paced
game, your players might expect something to happen when they press a key, and
become frustrated if it doesn't happen until they let go.  Other times, you may
want to change something (like a velocity) when a key is pressed, and put it
back the way it was when the key is released.

Here's some example code.  This program moves a circle around the screen when
the user presses the arrow keys:

    program = interactionOf(initial, step, event, picture)

    initial(rs) = (0, 0)

    step(point, dt) = point

    event((x, y), KeyPress("Up"))    = (x, y + 1)
    event((x, y), KeyPress("Down"))  = (x, y - 1)
    event((x, y), KeyPress("Left"))  = (x - 1, y)
    event((x, y), KeyPress("Right")) = (x + 1, y)
    event(point,  other)             = point

    picture(x, y) = translated(solidCircle(1/2), x, y)

Notice that the `event` function uses multiple constructors, to handle different
events.  This is a common way of doing things, just like with all data types
that use multiple constructors.  The last equation for the `event` function is
very important, too:  It tells the program what to do with any event not covered
by the first four `event` functions. Keep in mind that many things are events,
even if you don't intend for them to be.  Without the last `event` function, the
program would crash every time someone moves the mouse over the screen!

Keys are identified by `Text` values, so you need to know the name of a key to
recognize it.  These are some common keys used in interactions.

* For most keys that have a visible single-letter character, the key name is
  that character, in upper-case when it is a letter.  For example, "A", "B",
  "1", and "2" are key names.  So is " ", the text value containing just a space
  character.
* Other common names are "Enter", "Up", "Down", "Left", "Right", "Esc", "Shift",
  "Ctrl", "Alt", "Tab", and "F1" through "F12".

You can't memorize them all, though!  Instead, here's a program that will help
you find the name of any key on the keyboard:

    program = interactionOf(initial, step, event, text)
    initial(rs)              = ""
    step(s, dt)              = s
    event(s, KeyPress(name)) = name
    event(s,  other)         = s

(Notice that this program doesn't define its own `picture` function.  The
built-in function called `text` already does that job, so we can just use it!)

Pointer Events
----------------

The last three kinds of events, `PointerPress`, `PointerRelease`, and
`PointerMovement`, indicate that someone used a mouse, touchpad, or touch screen
to press, release, or move the pointer.  These events carry with them the x and
y positions of the pointer.

Here's a simple example that lets you draw polygons by clicking on vertices.

    program = interactionOf(initial, step, event, polyline)
    initial(rs)                      = []
    step(state, dt)                  = state
    event(state, PointerPress(x, y)) = state ++ [ (x, y) ]
    event(state, other)              = state

In some programs, you'll care if the click is in a specific part - for example,
on a button.  In that case, you may want to add guards to your event function,
like this:

    event(state, PointerPress(x, t))
      | x > -2, x < 2, y > 5, y < 7 = somethingExciting
    event(state, other) = state

Collaborations
==============

TODO: Write this section.

This section will discuss using the `collaborationOf` function in place of the `interactionOf` function,
which allows the creation of programs which accept input from multiple users.
