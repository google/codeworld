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

main = animationOf(scene)

scene(t) = ferrisWheel(t) & backdrop(t)

backdrop(t) = pictures[
    movingCloud(t),
    color(translate(solidRectangle(20, 4), 0, -8), light(green)),
    color(solidRectangle(20, 20), light(blue))
    ]

movingCloud(t) = translate(cloud, remainder(2 * t, 28) - 14, 8)

cloud = color(cloudParts, white)
  where cloudParts = translate(solidCircle(1.6),  0,  -0.4)
                   & translate(solidCircle(1.2), -1.2, 0.4)
                   & translate(solidCircle(1.2),  1,   0.2)

ferrisWheel(t) = pictures [
    movingPart(t),
    color(polygon[ (-8, -8), (0, -4), (8, -8) ], gray(0.5)),
    color(solidRectangle(0.4, 10), gray(0.3))
    ]

movingPart(t) =
    rotate(wheel, 30 * t) &
    pictures[ circularPath(car, 60 * a + 30 * t) | a <- [0 .. 5] ]

wheel =
    thickCircle(6, 0.4) &
    pictures[ rotate(solidRectangle(12, 0.2), 30 * a) | a <- [0 .. 5] ]

-- Rotate, translate, then rotate.  The result ends up not rotating at all in
-- balance, but moves along a circular path anyway.
circularPath(pic, a) = rotate(translate(rotate(pic, -a), 6, 0), a)

car = translate(solidRectangle(0.2, 1.2), 0, -0.5) &
      translate(solidRectangle(1.6, 0.8), 0, -1.2)
