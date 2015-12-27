{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

{-
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
-}

module Internal.Picture where

import qualified "base" Prelude as P
import Internal.Num
import Internal.Text
import Internal.Color

type Point = (Number, Number)
type Vector = (Number, Number)

addVectors :: (Vector, Vector) -> Vector
addVectors ((x1,y1), (x2,y2)) = (x1 + x2, y1 + y2)

vectorSum :: (Vector, Vector) -> Vector
vectorSum = addVectors

subtractVectors :: (Vector, Vector) -> Vector
subtractVectors ((x1,y1), (x2,y2)) = (x1 - x2, y1 - y2)

vectorDifference :: (Vector, Vector) -> Vector
vectorDifference = subtractVectors

scaleVector :: (Vector, Number) -> Vector
scaleVector ((x,y), k) = (k*x, k*y)

scaledVector :: (Vector, Number) -> Vector
scaledVector = scaleVector

rotateVector :: (Vector, Number) -> Vector
rotateVector ((x,y), angle) = (x * cos angle - y * sin angle,
                               x * sin angle + y * cos angle)

rotatedVector :: (Vector, Number) -> Vector
rotatedVector = rotateVector

dotProduct :: (Vector, Vector) -> Number
dotProduct ((x1,y1), (x2, y2)) = x1 * x2 + y1 * y2

{-# WARNING addVectors "Please use vectorSum(...) instead of addVectors(...)" #-}
{-# WARNING subtractVectors "Please use vectorDifference(...) instead of subtractVectors(...)" #-}
{-# WARNING scaleVector "Please use scaledVector(...) instead of scaleVector(...)" #-}
{-# WARNING rotateVector "Please use rotatedVector(...) instead of rotateVector(...)" #-}

data Picture = Polygon [Point]
             | Line [Point] !Number !P.Bool
             | Arc !Number !Number !Number !Number
             | Text !Text
             | Color !Color !Picture
             | Translate !Number !Number !Picture
             | Scale !Number !Number !Picture
             | Rotate !Number !Picture
             | Pictures [Picture]
             | Logo

instance P.Show Picture where show _ = "<<Picture>>"

-- | A blank picture
blank :: Picture
blank = Pictures []

-- | A thin line with these points as endpoints
line :: [Point] -> Picture
line ps = Line ps 0 P.False

-- | A thick line, with these endpoints, with this line width
thickLine :: ([Point], Number) -> Picture
thickLine (ps, n) = Line ps n P.False

-- | A thin polygon with these points as vertices
polygon :: [Point] -> Picture
polygon ps = Line ps 0 P.True

-- | A thin polygon with these points as vertices
thickPolygon :: ([Point], Number) -> Picture
thickPolygon (ps, n) = Line ps n P.True

-- | A solid polygon with these points as vertices
solidPolygon :: [Point] -> Picture
solidPolygon = Polygon

-- | A thin rectangle, with this width and height
rectangle :: (Number, Number) -> Picture
rectangle (w, h) = polygon [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)
    ]

-- | A solid rectangle, with this width and height
solidRectangle :: (Number, Number) -> Picture
solidRectangle (w, h) = solidPolygon [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)
    ]

-- | A thick rectangle, with this width and height and line width
thickRectangle :: (Number, Number, Number) -> Picture
thickRectangle (w, h, lw) = thickPolygon ([
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)
    ], lw)

-- | A thin circle, with this radius
circle :: Number -> Picture
circle r = arc (0, 360, r)

-- | A solid circle, with this radius
solidCircle :: Number -> Picture
solidCircle r = thickCircle (r/2, r)

-- | A thick circle, with this radius and line width
thickCircle :: (Number, Number) -> Picture
thickCircle (r, w) = Arc 0 360 r w

-- | A thin arc, starting and ending at these angles, with this radius
arc :: (Number, Number, Number) -> Picture
arc (b, e, r) = Arc b e r 0

-- | A solid sector of a circle (i.e., a pie slice) starting and ending at these
-- angles, with this radius
sector :: (Number, Number, Number) -> Picture
sector (b, e, r) = Arc b e (r/2) r

-- | A thick arc, starting and ending at these angles, with this radius and
-- line width
thickArc :: (Number, Number, Number, Number) -> Picture
thickArc (b, e, r, w) = Arc b e r w

-- | A piece of text
text :: Text -> Picture
text = Text

-- | A picture drawn entirely in this color.
color :: (Picture, Color) -> Picture
color (p, c) = Color c p

-- | A picture drawn entirely in this color.
colored :: (Picture, Color) -> Picture
colored = color

-- | A picture drawn entirely in this color.
coloured :: (Picture, Color) -> Picture
coloured = colored

-- | A picture drawn translated in these directions.
translate :: (Picture, Number, Number) -> Picture
translate (p, x, y) = Translate x y p

-- | A picture drawn translated in these directions.
translated :: (Picture, Number, Number) -> Picture
translated = translate

-- | A picture scaled by these factors.
scale :: (Picture, Number, Number) -> Picture
scale (p, x, y) = Scale x y p

-- | A picture scaled by these factors.
scaled :: (Picture, Number, Number) -> Picture
scaled = scale

-- | A picture scaled by these factors.
dilated :: (Picture, Number, Number) -> Picture
dilated = scaled

-- | A picture rotated by this angle.
rotate :: (Picture, Number) -> Picture
rotate (p, t) = Rotate t p

-- | A picture rotated by this angle.
rotated :: (Picture, Number) -> Picture
rotated = rotate

{-# WARNING color "Please use colored(...) instead of color(...)" #-}
{-# WARNING translate "Please use translated(...) instead of translate(...)" #-}
{-# WARNING rotate "Please use rotated(...) instead of rotate(...)" #-}
{-# WARNING scale "Please use scaled(...) instead of scale(...)" #-}

-- A picture made by drawing these pictures, ordered from top to bottom.
pictures :: [Picture] -> Picture
pictures = Pictures

-- Binary composition of pictures.
(&) :: Picture -> Picture -> Picture
infixr 0 &
a & Pictures bs = Pictures (a:bs)
a & b           = Pictures [a, b]

-- | A coordinate plane.  Adding this to your pictures can help you measure distances
-- more accurately.
--
-- Example:
--
--    main = pictureOf(myPicture & coordinatePlane)
--    myPicture = ...
coordinatePlane :: Picture
coordinatePlane = axes & numbers & guidelines
  where xline(y) = line[(-10, y), (10, y)]
        xaxis = color(xline(0), RGBA(0, 0, 0, 0.75))
        axes = xaxis & rotate(xaxis, 90)
        xguidelines = pictures[
            color(xline(k), RGBA(0, 0, 0, 0.25)) | k <- [-10, -9 .. 10] ]
        guidelines = xguidelines & rotate(xguidelines, 90)
        numbers = xnumbers & ynumbers
        xnumbers = pictures [
            translate(scale(text(printed(k)), 0.5, 0.5), k, 0.3)
            | k <- [-9, -8 .. 9], k P./= 0 ]
        ynumbers = pictures [
            translate(scale(text(printed(k)), 0.5, 0.5), 0.3, k)
            | k <- [-9, -8 .. 9], k P./= 0 ]

-- | The CodeWorld logo.
codeWorldLogo :: Picture
codeWorldLogo = Logo
