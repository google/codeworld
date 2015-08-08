Mandelbrot
==========

About the example
-----------------

This example draws a fractal known as the Mandelbrot set.  It is an advanced
example of pictures.  The main language features used are custom operators
and data types (for the complex numbers), and a crude way of drawing
bitmapped pictures.  The drawing is slow, but it works!

The program
-----------

    {-
        First, teach CodeWorld how to do arithmetic with complex numbers. I've
        chosen the follow operators:
          <+> for complex addition
          <*> for complex multiplication
    -}
    data Complex = C(Number, Number)
    C(x1, y1) <+> C(x2, y2) = C(x1    + x2   , y1    + y2   )
    C(x1, y1) <*> C(x2, y2) = C(x1*x2 - y1*y2, x1*y2 + y1*x2)

    {-
        Now, define the test for whether a point belongs to the set.  A
        point belongs to the Mandelbrot set if iterating the polynomial
        z -> z^2 + c, starting with z = 0, diverges.

        As a quick test for divergence, we check whether either coordinate
        exceeds 2.

        To get a better image, for points that do not belong to the set, we
        measure how many iterations were needed to detect divergence.  This
        number will be large for points close to the set.  Return Nothing for
        points in the set, and Just k for points that leave the target box in
        k iterations.
    -}
    diverged :: Complex -> Bool
    diverged (C(x, y)) = abs(x) > 2 || abs(y) > 2

    depth :: (Number, Complex, Complex, Number) -> Maybe Number
    depth(_, _, _, 0) = Nothing
    depth(m, z, c, k) | diverged(z) = Just(m)
                      | otherwise   = depth(m + 1, z <*> z <+> c, c, k - 1)

    {-
        Finally, render the set on a 500 by 500 pixel grid.  The source
        area is the intervals [-2, 2] on the real and imaginary axes.
        A bit of simple math maps that to the screen.
    -}
    mandelbrot :: (Number, Number) -> Picture
    mandelbrot(n, k) = rotate(scale(pic, 5, 5), 90)
      where pic = pictures[ spot(x, y, m) | x <- [-2, -2 + width .. 2],
                            y <- [-2, -2 + width .. 2],
                            Just(m) <- [depth(0, C(0, 0), C(x, y), k)] ]
            width         = 4 / n
            spot(x, y, m) = translate(shade(solidRectangle(width, width), m), x, y)
            shade(p, m)   = color(p, gray((1 - 1/m)^5))

    main :: Program
    main = pictureOf(mandelbrot(500, 25))

License
-------

Copyright 2015 Google Inc. All rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
