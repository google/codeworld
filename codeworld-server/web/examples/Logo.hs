{----- BEGIN LICENSE TEXT -----
  Copyright 2014 Google Inc. All rights reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
----- END LICENSE TEXT -----}
{-
    Example: Logo

    A CodeWorld logo built out of geometric patterns.  This shows off
    many of the capabilities of the picture mode.
-}

main = pictureOf logo

logo = pictures [
  translate(top,                    0,  6),
  translate(color(c, dark(green)), -6,  3),
  translate(color(o, dark(blue)),  -2,  3),
  translate(color(d, dark(yellow)), 2,  3),
  translate(color(e, dark(red)),    6,  3),
  translate(w,                     -8, -2),
  translate(o,                     -4, -2),
  translate(r,                      0, -2),
  translate(l,                      4, -2),
  translate(d,                      8, -2),
  translate(bottom,                 0, -6)
  ]

c = pictures [ rotate(line[(1.6, 0),(0,1.6)], th) | th <- [45, 55 .. 235]]
  & arc(60, 300, 1.6)

o = pictures [ rotate(scale(circle(1.6), 0.3, 1), th) | th <- [0, 15 .. 165] ]

d = translate(scale(parts, 1.5, 1), -1.2, 0)
  where parts = pictures [ arc(-90, 90, r) | r <- [0.2, 0.4 .. 1.6] ] &
                pictures [ rotate(line[ (0, 0), (1.6, 0) ], th) | th <- [-90, -80 .. 90] ]

e = go(8)
  where go(1) = blank
        go(n) = translate(arc(180, 270, 2), 1, 2 - phi)
              & translate(rotate(scale(go (n-1), 1/phi, 1/phi), 270), 0, 1)
              & color(rectangle(2, 2 * phi), gray 0.5)
        phi = (1 + sqrt 5) / 2

w = scale(design, 0.4, 1)
  where design = pictures[
                   translate(rotate(corner, 45), -2, -1.4),
                   translate(rotate(corner, 45),  2, -1.4)
                   ]
        corner = pictures [ line [ (x,0), (0,4-x) ] | x <- [ 0, 0.4 .. 4 ] ]

r = scale(design, 1.5, 1)
  where design = pictures[
                   translate(pictures[ leg(2,   k) | k <- [0,   5 ..  35] ], -0.6, 0.32),
                   translate(pictures[ leg(1.2, k) | k <- [40, 45 .. 180] ], -0.6, 0.32)
                   ]
        leg(r, k) = rotate(line [ (0,0), (0,-r) ], k)

l = go(5)
  where go(1) = blank
        go(n) = pictures[
                  solidRectangle(0.8, 1.2),
                  translate(scale(go(n-1), 0.95, 1/3), 0,  1.2),
                  translate(scale(go(n-1), 0.95, 1/3), 0, -1.2)
                  ]

top = go(5)
  where go(1) = line[(-4, 0), (4, 0)]
        go(n) = let sub = scale(go (n-1), 1/3, 1/3)
               in pictures[ translate(sub, -8/3, 0),
                            translate(sub,  8/3, 0),
                            translate(rotate(sub,  60), -2/3, 1.12),
                            translate(rotate(sub, 300),  2/3, 1.12) ]

bottom = scale(top, 1, -1)
