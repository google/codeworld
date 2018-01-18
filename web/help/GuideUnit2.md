Defining Functions
==================

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
