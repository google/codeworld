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
  translate (   0) ( 150) top,
  translate (-150) (  75) $ color (muted green)  c,
  translate ( -50) (  75) $ color (muted blue)   o,
  translate (  50) (  75) $ color (muted yellow) d,
  translate ( 150) (  75) $ color (muted red)    e,
  translate (-200) ( -50) w,
  translate (-100) ( -50) o,
  translate (   0) ( -50) r,
  translate ( 100) ( -50) l,
  translate ( 200) ( -50) d,
  translate (   0) (-150) bottom
  ]

c = pictures [ rotate th (line [(40, 0),(0,40)]) | th <- [45, 55 .. 235]]
  & arc 60 300 40

o = pictures [ rotate t $ scale 0.3 1 $ circle 40 | t <- [0, 15 .. 165] ]

d = translate (-30) 0 $ scale 1.5 1 $
    pictures [ arc (-90) 90 r | r <- [5, 10 .. 40] ] &
    pictures [ rotate th (line [ (0,0), (40,0) ]) | th <- [-90, -80 .. 90] ]

e = go 8
  where go 1 = blank
        go n = translate 25 (50 - 25 * phi) (arc 180 270 50)
             & translate 0 25 (rotate 270 (scale (1/phi) (1/phi) (go (n-1))))
             & color (gray 0.5) (rectangle 50 (50 * phi))
        phi = (1 + sqrt 5) / 2

w = scale 0.4 1 $ pictures [
      translate (-50) (-35) (rotate 45 corner),
      translate ( 50) (-35) (rotate 45 corner) ]
  where corner = pictures [ line [ (x,0), (0,100-x) ] | x <- [ 0, 10 .. 100 ] ]

r = scale 1.5 1 $ pictures [
      translate (-15) ( 8) (pictures [ leg 50 k | k <- [0, 5 ..  40] ]),
      translate (-15) (13) (pictures [ leg 30 k | k <- [0, 6 .. 180] ]) ]
  where leg r k = rotate k $ line [ (0,0), (0,-r) ]

l = go 5
  where go  1 = blank
        go  n = pictures [
                  solidRectangle 20 30,
                  translate 0 ( 30) $ scale 0.95 (1/3) $ go (n-1),
                  translate 0 (-30) $ scale 0.95 (1/3) $ go (n-1)
                ]

top = go 5
  where go 1 = line [(-100, 0), (100, 0)]
        go n = let sub = scale (1/3) (1/3) (go (n-1))
               in pictures [ translate (-200/3)  0 sub,
                             translate ( 200/3)  0 sub,
                             translate (-100/6) 28 (rotate  60 sub),
                             translate ( 100/6) 28 (rotate 300 sub) ]

bottom = scale 1 (-1) top
