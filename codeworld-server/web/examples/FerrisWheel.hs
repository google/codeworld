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
    color (light green) (translate 0 (-200) (solidRectangle 500 100)),
    color (light blue)  (solidRectangle 500 500)
    ]

movingCloud t = translate ((50 * t) `mod` 700 - 350) 200 cloud

cloud = color white (pictures [
    translate (  0) (-10) (solidCircle 40),
    translate (-30) ( 10) (solidCircle 30),
    translate ( 25) (  5) (solidCircle 30)
    ])

ferrisWheel t = pictures [
    movingPart t,
    color (gray 0.5) (polygon [ (-200, -200), (0, -100), (200, -200) ]),
    color (gray 0.3) (solidRectangle 10 250)
    ]

movingPart t =
    rotate (30 * t) wheel &
    pictures [ circularPath (60 * a + 30 * t) car | a <- [0 .. 5] ]

wheel =
    thickCircle 150 10 &
    pictures [ rotate (30 * a) (solidRectangle 300 5) | a <- [0 .. 5] ]

-- Rotate, translate, then rotate.  The result ends up not rotating at all in
-- balance, but moves along a circular path anyway.
circularPath a pic = rotate a (translate 150 0 (rotate (-a) pic))

car = translate 0 (-15) (solidRectangle  5 30) &
      translate 0 (-30) (solidRectangle 40 20)
