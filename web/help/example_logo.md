Logo
====

About the example
-----------------

This fancy CodeWorld logo is built out of geometric patterns.  It shows off
creating patterns with list comprehensions, as well as some other fun
tricks and capabilities of picture mode.

The program
-----------

    main = pictureOf logo

    logo = pictures([
      translated(top,                      0,  6),
      translated(colored(c, dark(green)), -6,  3),
      translated(colored(o, dark(blue)),  -2,  3),
      translated(colored(d, dark(yellow)), 2,  3),
      translated(colored(e, dark(red)),    6,  3),
      translated(w,                       -8, -2),
      translated(o,                       -4, -2),
      translated(r,                        0, -2),
      translated(l,                        4, -2),
      translated(d,                        8, -2),
      translated(bottom,                   0, -6)
      ])

    c = pictures([ rotated(line([(1.6, 0),(0,1.6)]), th) | th <- [45, 55 .. 235]])
      & arc(60, 300, 1.6)

    o = pictures([ rotated(scaled(circle(1.6), 0.3, 1), th) | th <- [0, 15 .. 165] ])

    d = translated(scaled(parts, 1.5, 1), -1.2, 0)
      where parts = pictures([ arc(-90, 90, r) | r <- [0.2, 0.4 .. 1.6] ]) &
                    pictures([ rotated(line([ (0, 0), (1.6, 0) ]), th) | th <- [-90, -80 .. 90] ])

    e = go(8)
      where go(1) = blank
            go(n) = translated(arc(180, 270, 2), 1, 2 - phi)
                  & translated(rotated(scaled(go (n-1), 1/phi, 1/phi), 270), 0, 1)
                  & colored(rectangle(2, 2 * phi), gray 0.5)
            phi = (1 + sqrt(5)) / 2

    w = scaled(design, 0.4, 1)
      where design = pictures([
                       translated(rotated(corner, 45), -2, -1.4),
                       translated(rotated(corner, 45),  2, -1.4)
                       ])
            corner = pictures([ line([ (x,0), (0,4-x) ]) | x <- [ 0, 0.4 .. 4 ] ])

    r = scaled(design, 1.5, 1)
      where design = pictures([
                       translated(pictures[ leg(2,   k) | k <- [0,   5 ..  35] ], -0.6, 0.32),
                       translated(pictures[ leg(1.2, k) | k <- [40, 45 .. 180] ], -0.6, 0.32)
                       ])
            leg(r, k) = rotated(line([ (0,0), (0,-r) ]), k)

    l = go(5)
      where go(1) = blank
            go(n) = pictures([
                      solidRectangle(0.8, 1.2),
                      translated(scaled(go(n-1), 0.95, 1/3), 0,  1.2),
                      translated(scaled(go(n-1), 0.95, 1/3), 0, -1.2)
                      ])

    top = go(5)
      where go(1) = line([(-4, 0), (4, 0)])
            go(n) = let sub = scaled(go (n-1), 1/3, 1/3)
                   in pictures([ translated(sub, -8/3, 0),
                                 translated(sub,  8/3, 0),
                                 translated(rotated(sub,  60), -2/3, 1.12),
                                 translated(rotated(sub, 300),  2/3, 1.12) ])

    bottom = scaled(top, 1, -1)

License
-------

Copyright 2015 The CodeWorld Authors. All rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
