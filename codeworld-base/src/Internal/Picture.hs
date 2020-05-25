{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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

import qualified "codeworld-api" CodeWorld as CW
import GHC.Stack
import Internal.Color
import Internal.Num
import Internal.Text
import "base" Prelude ((.), ($), map)

-- | A point in two dimensions.  A point is written with the x coordinate
-- first, and the y coordinate second.  For example, (3, -2) is the point
-- with x coordinate 3 a y coordinate -2.
type Point = (Number, Number)

toCWPoint :: Point -> CW.Point
toCWPoint (x, y) = (toDouble x, toDouble y)

fromCWPoint :: CW.Point -> Point
fromCWPoint (x, y) = (fromDouble x, fromDouble y)

-- | Moves a given point by given x and y offsets
translatedPoint :: (Point, Number, Number) -> Point
translatedPoint (p, x, y) =
    fromCWPoint (CW.translatedPoint (toDouble x) (toDouble y) (toCWPoint p))

-- | Rotates a given point by given angle, in degrees
rotatedPoint :: (Point, Number) -> Point
rotatedPoint (p, a) =
    fromCWPoint (CW.rotatedPoint (toDouble (pi * a / 180)) (toCWPoint p))

reflectedPoint :: (Point, Number) -> Point
reflectedPoint (p, a) =
    fromCWPoint (CW.reflectedPoint (toDouble (pi * a / 180)) (toCWPoint p))

-- | Scales a given point by given x and y scaling factor.  Scaling by a
-- negative factor also reflects across that axis.
scaledPoint :: (Point, Number, Number) -> Point
scaledPoint (p, x, y) =
    fromCWPoint (CW.scaledPoint (toDouble x) (toDouble y) (toCWPoint p))

-- | Dilates a given point by given uniform scaling factor.  Dilating by a
-- negative factor also reflects across the origin.
dilatedPoint :: (Point, Number) -> Point
dilatedPoint (p, k) =
    fromCWPoint (CW.dilatedPoint (toDouble k) (toCWPoint p))

-- | A two-dimensional vector
type Vector = (Number, Number)

toCWVect :: Vector -> CW.Vector
toCWVect = toCWPoint

fromCWVect :: CW.Vector -> Vector
fromCWVect = fromCWPoint

-- | The length of the given vector
vectorLength :: Vector -> Number
vectorLength v = fromDouble (CW.vectorLength (toCWVect v))

-- | The counter-clockwise angle, in degrees, that a given vector make with the X-axis
vectorDirection :: Vector -> Number
vectorDirection v = 180 / pi * fromDouble (CW.vectorDirection (toCWVect v))

-- | The sum of two vectors
vectorSum :: (Vector, Vector) -> Vector
vectorSum (v, w) = fromCWVect (CW.vectorSum (toCWVect v) (toCWVect w))

-- | The difference of two vectors
vectorDifference :: (Vector, Vector) -> Vector
vectorDifference (v, w) =
    fromCWVect (CW.vectorDifference (toCWVect v) (toCWVect w))

-- | Scales a given vector by a given scalar multiplier
scaledVector :: (Vector, Number) -> Vector
scaledVector (v, k) = fromCWVect (CW.scaledVector (toDouble k) (toCWVect v))

-- | Rotates a given vector by a given angle in degrees
rotatedVector :: (Vector, Number) -> Vector
rotatedVector (v, k) =
    fromCWVect (CW.rotatedVector (toDouble (pi * k / 180)) (toCWVect v))

-- | The dot product of two vectors
dotProduct :: (Vector, Vector) -> Number
dotProduct (v, w) = fromDouble (CW.dotProduct (toCWVect v) (toCWVect w))

-- | A type for pictures.
--
-- Pictures can be created from geometry functions such as 'circle'
-- and 'rectangle'.  They can be combined by overlaying them with '&'.
-- They can be modified using transformations like 'translated',
-- 'rotated', 'dilated', 'colored', and 'scaled'.  Ultimately, a picture
-- can be drawn on the screen using one of the CodeWorld entry points
-- such as 'drawingOf'.
newtype Picture = CWPic
    { toCWPic :: CW.Picture
    }

-- | A font in which lettering can be drawn.  Fonts are used with the
-- 'styledLettering' function.
--
-- 'NamedFont' may create a program that only works correctly on
-- computers where that font is installed, so you are encouraged to
-- use one of the other values.
data Font
    = Serif
    | SansSerif
    | Monospace
    | Handwriting
    | Fancy
    | NamedFont !Text

-- | A style in which lettering can be drawn.  Text styles are used
-- with the 'styledLettering' function.
data TextStyle
    = Plain
    | Italic
    | Bold

-- | A blank picture
blank :: HasCallStack => Picture
blank = withFrozenCallStack $ CWPic CW.blank

-- | A thin sequence of line segments with these endpoints
polyline :: HasCallStack => [Point] -> Picture
polyline ps = withFrozenCallStack $ CWPic (CW.polyline (map toCWVect ps))

-- | A thin sequence of line segments with these endpoints
path :: HasCallStack => [Point] -> Picture
path ps = withFrozenCallStack $ CWPic (CW.path (map toCWVect ps))

{-# WARNING path ["Please use polyline(...) instead of path(...).",
                  "path may be removed July 2020."] #-}

-- | A thin sequence of line segments, with these endpoints and line width
thickPolyline :: HasCallStack => ([Point], Number) -> Picture
thickPolyline (ps, n) = withFrozenCallStack $ CWPic (CW.thickPolyline (toDouble n) (map toCWVect ps))

-- | A thin sequence of line segments, with these endpoints and line width
thickPath :: HasCallStack => ([Point], Number) -> Picture
thickPath (ps, n) = withFrozenCallStack $ CWPic (CW.thickPath (toDouble n) (map toCWVect ps))

{-# WARNING thickPath ["Please use thickPolyline(...) instead of thickPath(...).",
                       "thickPath may be removed July 2020."] #-}

-- | A thin polygon with these points as vertices
polygon :: HasCallStack => [Point] -> Picture
polygon ps = withFrozenCallStack $ CWPic (CW.polygon (map toCWVect ps))

-- | A thin polygon with these points as vertices
thickPolygon :: HasCallStack => ([Point], Number) -> Picture
thickPolygon (ps, n) = withFrozenCallStack $ CWPic (CW.thickPolygon (toDouble n) (map toCWVect ps))

-- | A solid polygon with these points as vertices
solidPolygon :: HasCallStack => [Point] -> Picture
solidPolygon ps = withFrozenCallStack $ CWPic (CW.solidPolygon (map toCWVect ps))

-- | A thin curve passing through these points.
curve :: HasCallStack => [Point] -> Picture
curve ps = withFrozenCallStack $ CWPic (CW.curve (map toCWVect ps))

-- | A thick curve passing through these points, with this line width
thickCurve :: HasCallStack => ([Point], Number) -> Picture
thickCurve (ps, n) = withFrozenCallStack $ CWPic (CW.thickCurve (toDouble n) (map toCWVect ps))

-- | A thin closed curve passing through these points.
closedCurve :: HasCallStack => [Point] -> Picture
closedCurve ps = withFrozenCallStack $ CWPic (CW.closedCurve (map toCWVect ps))

-- | A thick closed curve passing through these points, with this line width.
thickClosedCurve :: HasCallStack => ([Point], Number) -> Picture
thickClosedCurve (ps, n) = withFrozenCallStack $ CWPic (CW.thickClosedCurve (toDouble n) (map toCWVect ps))

-- | A solid closed curve passing through these points.
solidClosedCurve :: HasCallStack => [Point] -> Picture
solidClosedCurve ps = CWPic (CW.solidClosedCurve (map toCWVect ps))

-- | A thin rectangle, with this width and height
rectangle :: HasCallStack => (Number, Number) -> Picture
rectangle (w, h) = withFrozenCallStack $ CWPic (CW.rectangle (toDouble w) (toDouble h))

-- | A solid rectangle, with this width and height
solidRectangle :: HasCallStack => (Number, Number) -> Picture
solidRectangle (w, h) = withFrozenCallStack $ CWPic (CW.solidRectangle (toDouble w) (toDouble h))

-- | A thick rectangle, with this width and height and line width
thickRectangle :: HasCallStack => (Number, Number, Number) -> Picture
thickRectangle (w, h, lw) =
    withFrozenCallStack $ CWPic (CW.thickRectangle (toDouble lw) (toDouble w) (toDouble h))

-- | A thin circle, with this radius
circle :: HasCallStack => Number -> Picture
circle r = withFrozenCallStack $ CWPic (CW.circle (toDouble r))

-- | A solid circle, with this radius
solidCircle :: HasCallStack => Number -> Picture
solidCircle r = withFrozenCallStack $ CWPic (CW.solidCircle (toDouble r))

-- | A thick circle, with this radius and line width
thickCircle :: HasCallStack => (Number, Number) -> Picture
thickCircle (r, w) = withFrozenCallStack $ CWPic (CW.thickCircle (toDouble w) (toDouble r))

-- | A thin arc, starting and ending at these angles, with this radius
arc :: HasCallStack => (Number, Number, Number) -> Picture
arc (b, e, r) = withFrozenCallStack $ CWPic
    (CW.arc (toDouble (pi * b / 180)) (toDouble (pi * e / 180)) (toDouble r))

-- | A solid sector of a circle (i.e., a pie slice) starting and ending at these
-- angles, with this radius
sector :: HasCallStack => (Number, Number, Number) -> Picture
sector (b, e, r) = withFrozenCallStack $ CWPic
    (CW.sector
         (toDouble (pi * b / 180))
         (toDouble (pi * e / 180))
         (toDouble r))

-- | A thick arc, starting and ending at these angles, with this radius and
-- line width
thickArc :: HasCallStack => (Number, Number, Number, Number) -> Picture
thickArc (b, e, r, w) = withFrozenCallStack $ CWPic
    (CW.thickArc
        (toDouble w)
        (toDouble (pi * b / 180))
        (toDouble (pi * e / 180))
        (toDouble r))

-- | A rendering of text characters.
lettering :: HasCallStack => Text -> Picture
lettering t = withFrozenCallStack $ CWPic (CW.lettering (fromCWText t))

-- | A rendering of text characters.
text :: HasCallStack => Text -> Picture
text t = withFrozenCallStack $ CWPic (CW.lettering (fromCWText t))

{-# WARNING text ["Please use lettering(...) instead of text(...).",
                  "text may be removed July 2020."] #-}

-- | A rendering of text characters, with a specific choice of font and style.
styledLettering :: HasCallStack => (Text, Font, TextStyle) -> Picture
styledLettering (t, f, s) =
    withFrozenCallStack $ CWPic (CW.styledLettering (fromCWStyle s) (fromCWFont f) (fromCWText t))
  where
    fromCWStyle Plain = CW.Plain
    fromCWStyle Bold = CW.Bold
    fromCWStyle Italic = CW.Italic
    fromCWFont Serif = CW.Serif
    fromCWFont SansSerif = CW.SansSerif
    fromCWFont Monospace = CW.Monospace
    fromCWFont Handwriting = CW.Handwriting
    fromCWFont Fancy = CW.Fancy
    fromCWFont (NamedFont fnt) = CW.NamedFont (fromCWText fnt)

-- | A rendering of text characters, with a specific choice of font and style.
styledText :: HasCallStack => (Text, Font, TextStyle) -> Picture
styledText args = withFrozenCallStack $ styledLettering args

{-# WARNING styledText ["Please use styledLettering(...) instead of styledText(...).",
                        "styledText may be removed July 2020."] #-}

-- | A picture drawn entirely in this color.
colored :: HasCallStack => (Picture, Color) -> Picture
colored (p, c) = withFrozenCallStack $ CWPic (CW.colored (toCWColor c) (toCWPic p))

-- | A picture drawn entirely in this color.
coloured :: HasCallStack => (Picture, Color) -> Picture
coloured args = withFrozenCallStack $ colored args

-- | A picture drawn translated in these directions.
translated :: HasCallStack => (Picture, Number, Number) -> Picture
translated (p, x, y) =
    withFrozenCallStack $ CWPic (CW.translated (toDouble x) (toDouble y) (toCWPic p))

-- | A picture scaled by these factors in the x and y directions.
-- Factors greater than @1@ stretch the picture larger in that direction, and
-- less than @1@ squish it smaller in that direction.
--
-- Negative factors also reflect the picture across that axis. For example, to mirror
-- a picture over the x-axis: @scaled(p, -1, 1)@.
scaled :: HasCallStack => (Picture, Number, Number) -> Picture
scaled (p, x, y) = withFrozenCallStack $ CWPic (CW.scaled (toDouble x) (toDouble y) (toCWPic p))

-- | A picture with both dimensions scaled by the given factor.
-- Factors greater than @1@ make the picture bigger and less than @1@
-- make it smaller.
--
-- Negative factors reflect the picture across the origin, which is the same as
-- rotating it by 180 degrees.
dilated :: HasCallStack => (Picture, Number) -> Picture
dilated (p, k) = withFrozenCallStack $ CWPic (CW.dilated (toDouble k) (toCWPic p))

-- | A picture rotated by this angle.
rotated :: HasCallStack => (Picture, Number) -> Picture
rotated (p, th) = withFrozenCallStack $ CWPic (CW.rotated (toDouble (pi * th / 180)) (toCWPic p))

-- | A picture reflected across a line through the origin at this angle, in
-- degrees.  For example, an angle of 0 reflects the picture vertically
-- across the x axis, while an angle of 90 reflects the picture
-- horizontally across the y axis, and an agle of 45 reflects the picture
-- across the main diagonal.
reflected :: HasCallStack => (Picture, Number) -> Picture
reflected (p, th) = withFrozenCallStack $ CWPic (CW.reflected (toDouble (pi * th / 180)) (toCWPic p))

-- | A picture clipped to a rectangle of this width and height.
clipped :: HasCallStack => (Picture, Number, Number) -> Picture
clipped (p, w, h) =
    withFrozenCallStack $ CWPic (CW.clipped (toDouble w) (toDouble h) (toCWPic p))

-- | A picture made by drawing this list of pictures, ordered from front to back.
pictures :: HasCallStack => [Picture] -> Picture
pictures ps = withFrozenCallStack $ CWPic (CW.pictures (map toCWPic ps))

-- | A binary operation that overlays one picture in from of the other.
(&) :: HasCallStack => Picture -> Picture -> Picture
infixr 0 &

a & b = withFrozenCallStack $ CWPic (toCWPic a CW.& toCWPic b)

-- | A coordinate plane.
--
-- Adding this to your pictures can help you measure distances
-- more accurately.
--
-- Example:
--
-- > program = drawingOf(myPicture & coordinatePlane)
-- > myPicture = ...
coordinatePlane :: HasCallStack => Picture
coordinatePlane = withFrozenCallStack $ CWPic CW.coordinatePlane

-- | The CodeWorld logo.
codeWorldLogo :: HasCallStack => Picture
codeWorldLogo = withFrozenCallStack $ CWPic CW.codeWorldLogo
