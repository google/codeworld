Interactions
============

We've learned to do a lot of cool things with our programs by now, but there's
one thing missing: interaction! We can't make games if people can't interact
with our programs. We can create programs that people can interact with by
using the `interactionOf` function in place of the `simulationOf` function, 
which allows you to decide what will happen when someone presses a key or clicks
the mouse while your program is running.

Interactions are very similar to simulations in form. The `initial`, `step`,
and `picture` parts of the interaction are the same as in a simulation. The only
difference is that interactions have an extra parameter, commonly called `event`. The `event`
function fits into `interactionOf` between the `step` and `picture` functions, so
while the first line of a simulation looks like this:

    program          = simulationOf(initial, step, picture)

the first line of an interaction looks like this:

    program          = interactionOf(initial, step, event, picture)

Here's how the `event` part of an interaction fits in with the parts we are familiar
with from simulations.

The Event function
=====================

As we have already said, the `event` function tells the computer what to do when someone
presses a key or clicks the mouse while your program is running. You can define `event`
to not make any changes when an event happens, like the program below:

    program          = interactionOf(initial, step, event, picture)
    initial(rs) = -10
    step(x, dt) = x + dt
    event(x, e) = x
    picture(x)  = translated(solidCircle(1/2), x, 0)

This `event` function returns the same value for any event that occurs.  As a result, it
functions exactly as a simulation would.  That's not a very interesting interaction, though.
In order to make a more interesting program, we need to know how to handle different types
of events. 

We haven't seen events yet, so let's take a to look at the types of events we might deal with.

Event Types
----------------

* KeyPress :: Text -> Event
* KeyRelease :: Text -> Event
* PointerPress :: Point -> Event
* PointerRelease :: Point -> Event
* PointerMovement :: Point -> Event

Key Events
----------------

The first two types of events above, `KeyPress` and `KeyRelease`, deal with what happens when
someone presses a key.  `KeyPress` tells the program what to do when someone presses a key,
while `KeyRelease` tells the program what to fo when a key is released. These are slightly
different, so it's important to consider which one you want. You don't want your players
expecting something to happen when they press a key and thinking the game is broken when
all they have to do is release the key.

Here's a program that let's you move a circle around the screen with the arrow keys:

    program = interactionOf(initial, step, event, picture)
    initial(rs) = (0, 0)
    step(point, dt) = point
    event((x, y), KeyPress("Up"))    = (x, y + 1)
    event((x, y), KeyPress("Down"))  = (x, y - 1)
    event((x, y), KeyPress("Left"))  = (x - 1, y)
    event((x, y), KeyPress("Right")) = (x + 1, y)
    event(point,  other)             = point
    picture(x, y) = translated(solidCircle(1/2), x, y)

The final `event` function in the program above is very important: It tells the program
what to do with any event not covered by the first four `event` functions. Keep in mind that
many things are events, even if you don't intend for them to be.  Without the last `event`
function, your program will crash every time someone moves a mouse pointer across the screen!

These are some common keys used in interactions

* For most keys that have a visible single-letter character, the key name is that character, in
upper-case when it is a letter.  For example, “A”, “B”, “1”, and “2” are key names.  So is “ ”,
the text value containing just a space character.

* Other common names are “Enter”, “Up”, “Down”, “Left”, “Right”, “Esc”, “Shift”, “Ctrl”, “Alt”,
“Tab”, and “F1” through “F12”.

Here's a program that will help you find out the name of any key on the keyboard:

    program = interactionOf(initial, step, event, text)
    initial(rs) = ""
    step(s, dt) = s
    event(s, KeyPress(name))    = name
    event(s,  other)             = s

Pointer Events
----------------

The last three types of events, `PointerPress`, `PointerRelease`, and `PointerMovement`, deal with
what happens when someone uses a mouse, touchpad, or touch screen to press, release, or move the
pointer.  These events require the state to remember the x and y positions of the pointer and of
anything which reacts to a pointer event in a tuple.  The `event` functions will need to have an
argument for the current position of the pointer on the screen.

Collaborations
==============

TODO: Write this section.

This section will discuss using the `collaborationOf` function in place of the `interactionOf` function,
which allows the creation of programs which accept input from multiple users.
