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
    Example: FerrisWheel

    An example of an animation.  An animation is a function from time to a
    Picture.  The parameter, normally called t, is the total time that's passed
    in seconds.
-}

main = animationOf scene

scene t = ferrisWheel t & backdrop t

backdrop t = pictures [
    movingCloud t,
    color (light green) (translate 0 (-8) (solidRectangle 20 4)),
    color (light blue)  (solidRectangle 20 20)
    ]

movingCloud t = translate ((2 * t) `mod` 28 - 14) 8 cloud

cloud = color white (pictures [
    translate ( 0  ) (-0.4) (solidCircle 1.6),
    translate (-1.2) ( 0.4) (solidCircle 1.2),
    translate ( 1  ) ( 0.2) (solidCircle 1.2)
    ])

ferrisWheel t = pictures [
    movingPart t,
    color (gray 0.5) (polygon [ (-8, -8), (0, -4), (8, -8) ]),
    color (gray 0.3) (solidRectangle 0.4 10)
    ]

movingPart t =
    rotate (30 * t) wheel &
    pictures [ circularPath (60 * a + 30 * t) car | a <- [0 .. 5] ]

wheel =
    thickCircle 6 0.4 &
    pictures [ rotate (30 * a) (solidRectangle 12 0.2) | a <- [0 .. 5] ]

-- Rotate, translate, then rotate.  The result ends up not rotating at all in
-- balance, but moves along a circular path anyway.
circularPath a pic = rotate a (translate 6 0 (rotate (-a) pic))

car = translate 0 (-0.5) (solidRectangle 0.2 1.2) &
      translate 0 (-1.2) (solidRectangle 1.6 0.8)
