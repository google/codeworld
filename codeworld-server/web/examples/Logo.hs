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
  translate ( 0) ( 6) top,
  translate (-6) ( 3) $ color (muted green)  c,
  translate (-2) ( 3) $ color (muted blue)   o,
  translate ( 2) ( 3) $ color (muted yellow) d,
  translate ( 6) ( 3) $ color (muted red)    e,
  translate (-8) (-2) w,
  translate (-4) (-2) o,
  translate ( 0) (-2) r,
  translate ( 4) (-2) l,
  translate ( 8) (-2) d,
  translate ( 0) (-6) bottom
  ]

c = pictures [ rotate th (line [(1.6, 0),(0,1.6)]) | th <- [45, 55 .. 235]]
  & arc 60 300 1.6

o = pictures [ rotate th $ scale 0.3 1 $ circle 1.6 | th <- [0, 15 .. 165] ]

d = translate (-1.2) 0 $ scale 1.5 1 $
    pictures [ arc (-90) 90 r | r <- [0.2, 0.4 .. 1.6] ] &
    pictures [ rotate th (line [ (0, 0), (1.6, 0) ]) | th <- [-90, -80 .. 90] ]

e = go 8
  where go 1 = blank
        go n = translate 1 (2 - phi) (arc 180 270 2)
             & translate 0 1 (rotate 270 (scale (1/phi) (1/phi) (go (n-1))))
             & color (gray 0.5) (rectangle 2 (2 * phi))
        phi = (1 + sqrt 5) / 2

w = scale 0.4 1 $ pictures [
      translate (-2) (-1.4) (rotate 45 corner),
      translate ( 2) (-1.4) (rotate 45 corner) ]
  where corner = pictures [ line [ (x,0), (0,4-x) ] | x <- [ 0, 0.4 .. 4 ] ]

r = scale 1.5 1 $ pictures [
      translate (-0.6) (0.32) (pictures [ leg 2   k | k <- [0,   5 .. 35] ]),
      translate (-0.6) (0.32) (pictures [ leg 1.2 k | k <- [40, 45 .. 180] ]) ]
  where leg r k = rotate k $ line [ (0,0), (0,-r) ]

l = go 5
  where go  1 = blank
        go  n = pictures [
                  solidRectangle 0.8 1.2,
                  translate 0 ( 1.2) $ scale 0.95 (1/3) $ go (n-1),
                  translate 0 (-1.2) $ scale 0.95 (1/3) $ go (n-1)
                ]

top = go 5
  where go 1 = line [(-4, 0), (4, 0)]
        go n = let sub = scale (1/3) (1/3) (go (n-1))
               in pictures [ translate (-8/3) 0    sub,
                             translate ( 8/3) 0    sub,
                             translate (-2/3) 1.12 (rotate  60 sub),
                             translate ( 2/3) 1.12 (rotate 300 sub) ]

bottom = scale 1 (-1) top
