{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-
  Copyright 2020 The CodeWorld Authors. All rights reserved.

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
module CodeWorld.Picture where

import CodeWorld.Color
import Control.DeepSeq
import Data.List
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Stack
import Util.EmbedAsUrl

-- | A point in two dimensions.  A point is written with the x coordinate
-- first, and the y coordinate second.  For example, (3, -2) is the point
-- with x coordinate 3 a y coordinate -2.
type Point = (Double, Double)

-- | Moves a given point by given x and y offsets
--
-- >>> translatedPoint 1 2 (10, 10)
-- (11.0, 12.0)
-- >>> translatedPoint (-1) (-2) (0, 0)
-- (-1.0, -2.0)
translatedPoint :: Double -> Double -> Point -> Point
translatedPoint tx ty (x, y) = (x + tx, y + ty)

-- | Rotates a given point by given angle, in radians
--
-- >>> rotatedPoint 45 (10, 0)
-- (7.071, 7.071)
rotatedPoint :: Double -> Point -> Point
rotatedPoint = rotatedVector

-- | Reflects a given point across a line through the origin at this
-- angle, in radians.  For example, an angle of 0 reflects the point
-- vertically across the x axis, while an angle of @pi / 2@ reflects the
-- point horizontally across the y axis.
reflectedPoint :: Double -> Point -> Point
reflectedPoint th (x, y) = (x * cos a + y * sin a, x * sin a - y * cos a)
  where
    a = 2 * th

-- | Scales a given point by given x and y scaling factor.  Scaling by a
-- negative factor also reflects across that axis.
--
-- >>> scaledPoint 2 3 (10, 10)
-- (20, 30)
scaledPoint :: Double -> Double -> Point -> Point
scaledPoint kx ky (x, y) = (kx * x, ky * y)

-- | Dilates a given point by given uniform scaling factor.  Dilating by a
-- negative factor also reflects across the origin.
--
-- >>> dilatedPoint 2 (10, 10)
-- (20, 20)
dilatedPoint :: Double -> Point -> Point
dilatedPoint k (x, y) = (k * x, k * y)

-- | A two-dimensional vector
type Vector = (Double, Double)

-- | The length of the given vector.
--
-- >>> vectorLength (10, 10)
-- 14.14
vectorLength :: Vector -> Double
vectorLength (x, y) = sqrt (x * x + y * y)

-- | The counter-clockwise angle, in radians, that a given vector make with the X-axis
--
-- >>> vectorDirection (1,0)
-- 0.0
-- >>> vectorDirection (1,1)
-- 0.7853981633974483
-- >>> vectorDirection (0,1)
-- 1.5707963267948966
vectorDirection :: Vector -> Double
vectorDirection (x, y) = atan2 y x

-- | The sum of two vectors
vectorSum :: Vector -> Vector -> Vector
vectorSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | The difference of two vectors
vectorDifference :: Vector -> Vector -> Vector
vectorDifference (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- | Scales a given vector by a given scalar multiplier.
--
-- >>> scaledPoint 2 (10, 10)
-- (20, 20)
scaledVector :: Double -> Vector -> Vector
scaledVector k (x, y) = (k * x, k * y)

-- | Rotates a given vector by a given angle in radians
--
-- >>> rotatedVector pi (1.0, 0.0)
-- (-1.0, 1.2246467991473532e-16)
-- >>> rotatedVector (pi / 2) (1.0, 0.0)
-- (6.123233995736766e-17, 1.0)
rotatedVector :: Double -> Vector -> Vector
rotatedVector angle (x, y) =
  (x * cos angle - y * sin angle, x * sin angle + y * cos angle)

-- | The dot product of two vectors
dotProduct :: Vector -> Vector -> Double
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- | A design, diagram, or drawing that can be displayed and seen.
-- In technical terms, a picture is an assignment of a color to
-- every point of the coordinate plane.  CodeWorld contains functions
-- to create pictures from simple geometry primitives, to transform
-- existing pictures, and to combine simpler pictures into more
-- complex compositions.
--
-- Ultimately, a picture can be drawn on the screen using one of the
-- CodeWorld entry points such as 'drawingOf'.
data Picture
  = SolidPolygon (Maybe SrcLoc) [Point]
  | SolidClosedCurve (Maybe SrcLoc) [Point]
  | Polygon (Maybe SrcLoc) [Point]
  | ThickPolygon (Maybe SrcLoc) [Point] !Double
  | Rectangle (Maybe SrcLoc) !Double !Double
  | SolidRectangle (Maybe SrcLoc) !Double !Double
  | ThickRectangle (Maybe SrcLoc) !Double !Double !Double
  | ClosedCurve (Maybe SrcLoc) [Point]
  | ThickClosedCurve (Maybe SrcLoc) [Point] !Double
  | Polyline (Maybe SrcLoc) [Point]
  | ThickPolyline (Maybe SrcLoc) [Point] !Double
  | Curve (Maybe SrcLoc) [Point]
  | ThickCurve (Maybe SrcLoc) [Point] !Double
  | Circle (Maybe SrcLoc) !Double
  | SolidCircle (Maybe SrcLoc) !Double
  | ThickCircle (Maybe SrcLoc) !Double !Double
  | Sector (Maybe SrcLoc) !Double !Double !Double
  | Arc (Maybe SrcLoc) !Double !Double !Double
  | ThickArc (Maybe SrcLoc) !Double !Double !Double !Double
  | StyledLettering (Maybe SrcLoc) !TextStyle !Font !Text
  | Lettering (Maybe SrcLoc) !Text
  | Color (Maybe SrcLoc) !Color !Picture
  | Translate (Maybe SrcLoc) !Double !Double !Picture
  | Scale (Maybe SrcLoc) !Double !Double !Picture
  | Dilate (Maybe SrcLoc) !Double !Picture
  | Rotate (Maybe SrcLoc) !Double !Picture
  | Reflect (Maybe SrcLoc) !Double !Picture
  | Clip (Maybe SrcLoc) !Double !Double !Picture
  | CoordinatePlane (Maybe SrcLoc)
  | Sketch (Maybe SrcLoc) !Text !Text !Double !Double
  | Pictures (Maybe SrcLoc) [Picture]
  | PictureAnd (Maybe SrcLoc) [Picture]
  | Blank (Maybe SrcLoc)
  deriving (Generic)

instance NFData Picture

-- A style in which to draw lettering.  Either 'Plain', 'Bold', or
-- 'Italic'
data TextStyle
  = -- | Plain letters with no style
    Plain
  | -- | Heavy, thick lettering used for emphasis
    Bold
  | -- | Slanted script-like lettering used for emphasis
    Italic
  deriving (Generic, Show)

instance NFData TextStyle

-- A font in which to draw lettering.  There are several built-in
-- font families ('SansSerif', 'Serif', 'Monospace', 'Handwriting',
-- and 'Fancy') that can look different on each screen.  'NamedFont'
-- can be used for a specific font.  However, if the font is not
-- installed on the computer running your program, a different font
-- may be used instead.
data Font
  = SansSerif
  | Serif
  | Monospace
  | Handwriting
  | Fancy
  | NamedFont !Text
  deriving (Generic, Show)

instance NFData Font

-- | A blank picture
blank :: HasCallStack => Picture
blank = Blank (getDebugSrcLoc callStack)

-- | A thin sequence of line segments, with these points as endpoints
polyline :: HasCallStack => [Point] -> Picture
polyline ps = Polyline (getDebugSrcLoc callStack) ps

-- | A thick sequence of line segments, with given line width and endpoints
thickPolyline :: HasCallStack => Double -> [Point] -> Picture
thickPolyline n ps = ThickPolyline (getDebugSrcLoc callStack) ps n

-- | A thin polygon with these points as vertices
polygon :: HasCallStack => [Point] -> Picture
polygon ps = Polygon (getDebugSrcLoc callStack) ps

-- | A thick polygon with this line width and these points as
-- vertices
thickPolygon :: HasCallStack => Double -> [Point] -> Picture
thickPolygon n ps = ThickPolygon (getDebugSrcLoc callStack) ps n

-- | A solid polygon with these points as vertices
solidPolygon :: HasCallStack => [Point] -> Picture
solidPolygon ps = SolidPolygon (getDebugSrcLoc callStack) ps

-- | A smooth curve passing through these points.
curve :: HasCallStack => [Point] -> Picture
curve ps = Curve (getDebugSrcLoc callStack) ps

-- | A thick smooth curve with this line width, passing through these points.
thickCurve :: HasCallStack => Double -> [Point] -> Picture
thickCurve n ps = ThickCurve (getDebugSrcLoc callStack) ps n

-- | A smooth closed curve passing through these points.
closedCurve :: HasCallStack => [Point] -> Picture
closedCurve ps = ClosedCurve (getDebugSrcLoc callStack) ps

-- | A thick smooth closed curve with this line width, passing through these points.
thickClosedCurve :: HasCallStack => Double -> [Point] -> Picture
thickClosedCurve n ps = ThickClosedCurve (getDebugSrcLoc callStack) ps n

-- | A solid smooth closed curve passing through these points.
solidClosedCurve :: HasCallStack => [Point] -> Picture
solidClosedCurve ps = SolidClosedCurve (getDebugSrcLoc callStack) ps

rectangleVertices :: Double -> Double -> [Point]
rectangleVertices w h = [(w / 2, h / 2), (w / 2, - h / 2), (- w / 2, - h / 2), (- w / 2, h / 2)]

-- | A thin rectangle, with this width and height
rectangle :: HasCallStack => Double -> Double -> Picture
rectangle w h = Rectangle (getDebugSrcLoc callStack) w h

-- | A solid rectangle, with this width and height
solidRectangle :: HasCallStack => Double -> Double -> Picture
solidRectangle w h = SolidRectangle (getDebugSrcLoc callStack) w h

-- | A thick rectangle, with this line width, and width and height
thickRectangle :: HasCallStack => Double -> Double -> Double -> Picture
thickRectangle lw w h = ThickRectangle (getDebugSrcLoc callStack) lw w h

-- | A thin circle, with this radius
circle :: HasCallStack => Double -> Picture
circle = Circle (getDebugSrcLoc callStack)

-- | A thick circle, with this line width and radius
thickCircle :: HasCallStack => Double -> Double -> Picture
thickCircle = ThickCircle (getDebugSrcLoc callStack)

-- | A thin arc, starting and ending at these angles, with this radius
--
-- Angles are in radians.
arc :: HasCallStack => Double -> Double -> Double -> Picture
arc b e r = Arc (getDebugSrcLoc callStack) b e r

-- | A thick arc with this line width, starting and ending at these angles,
-- with this radius.
--
-- Angles are in radians.
thickArc :: HasCallStack => Double -> Double -> Double -> Double -> Picture
thickArc w b e r = ThickArc (getDebugSrcLoc callStack) b e r w

-- | A solid circle, with this radius
solidCircle :: HasCallStack => Double -> Picture
solidCircle = SolidCircle (getDebugSrcLoc callStack)

-- | A solid sector of a circle (i.e., a pie slice) starting and ending at these
-- angles, with this radius
--
-- Angles are in radians.
sector :: HasCallStack => Double -> Double -> Double -> Picture
sector = Sector (getDebugSrcLoc callStack)

-- | A rendering of text characters.
lettering :: HasCallStack => Text -> Picture
lettering = Lettering (getDebugSrcLoc callStack)

-- | A rendering of text characters onto a Picture, with a specific
-- choice of font and style.
styledLettering :: HasCallStack => TextStyle -> Font -> Text -> Picture
styledLettering = StyledLettering (getDebugSrcLoc callStack)

-- | A picture drawn entirely in this color.
colored :: HasCallStack => Color -> Picture -> Picture
colored = Color (getDebugSrcLoc callStack)

-- | A picture drawn entirely in this colour.
coloured :: HasCallStack => Color -> Picture -> Picture
coloured = colored

-- | A picture drawn translated in these directions.
translated :: HasCallStack => Double -> Double -> Picture -> Picture
translated = Translate (getDebugSrcLoc callStack)

-- | A picture scaled by these factors in the x and y directions.  Scaling
-- by a negative factoralso reflects across that axis.
scaled :: HasCallStack => Double -> Double -> Picture -> Picture
scaled = Scale (getDebugSrcLoc callStack)

-- | A picture scaled uniformly in all directions by this scale factor.
-- Dilating by a negative factor also reflects across the origin.
dilated :: HasCallStack => Double -> Picture -> Picture
dilated = Dilate (getDebugSrcLoc callStack)

-- | A picture rotated by this angle about the origin.
--
-- Angles are in radians.
rotated :: HasCallStack => Double -> Picture -> Picture
rotated = Rotate (getDebugSrcLoc callStack)

-- | A picture reflected across a line through the origin at this angle, in
-- radians.  For example, an angle of 0 reflects the picture vertically
-- across the x axis, while an angle of @pi / 2@ reflects the picture
-- horizontally across the y axis.
reflected :: HasCallStack => Double -> Picture -> Picture
reflected = Reflect (getDebugSrcLoc callStack)

-- | A picture clipped to a rectangle around the origin with this width and height.
clipped :: HasCallStack => Double -> Double -> Picture -> Picture
clipped = Clip (getDebugSrcLoc callStack)

-- A picture made by drawing these pictures, ordered from top to bottom.
pictures :: HasCallStack => [Picture] -> Picture
pictures = Pictures (getDebugSrcLoc callStack)

-- | Binary composition of pictures.
(&) :: HasCallStack => Picture -> Picture -> Picture

infixr 0 &

a & PictureAnd loc2 bs
  | srcContains loc1 loc2 = PictureAnd loc1 (a : bs)
  where
    loc1 = getDebugSrcLoc callStack
a & b = PictureAnd (getDebugSrcLoc callStack) [a, b]

instance Monoid Picture where
  mempty = blank
  mconcat = pictures

instance Semigroup Picture where
  (<>) = (&)

-- | A coordinate plane.  Adding this to your pictures can help you measure distances
-- more accurately.
--
-- Example:
-- @
-- main = drawingOf (myPicture <> coordinatePlane)
-- myPicture = ...
-- @
coordinatePlane :: HasCallStack => Picture
coordinatePlane = CoordinatePlane (getDebugSrcLoc callStack)

-- | The CodeWorld logo.
codeWorldLogo :: HasCallStack => Picture
codeWorldLogo =
  Sketch
    (getDebugSrcLoc callStack)
    "codeWorldLogo"
    $(embedAsUrl "image/svg+xml" "data/codeworld.svg")
    17.68
    7.28

-- | An image from a standard image format.  The image can be any universally
-- supported format, including SVG, PNG, JPG, etc.  SVG should be preferred, as
-- it behaves better with transformations.
image ::
  HasCallStack =>
  -- | Name for the picture, used for debugging
  Text ->
  -- | Data-scheme URI for the image data
  Text ->
  -- | Width, in CodeWorld screen units
  Double ->
  -- | Height, in CodeWorld screen units
  Double ->
  Picture
image = Sketch (getDebugSrcLoc callStack)

getDebugSrcLoc :: CallStack -> Maybe SrcLoc
getDebugSrcLoc cs = Data.List.find ((== "main") . srcLocPackage) locs
  where
    locs = map snd (getCallStack cs)

srcContains :: Maybe SrcLoc -> Maybe SrcLoc -> Bool
srcContains Nothing _ = False
srcContains _ Nothing = True
srcContains (Just a) (Just b) =
  srcLocFile a == srcLocFile b && srcLocStartLine a < srcLocStartLine b
    || ( srcLocStartLine a == srcLocStartLine b
           && srcLocStartCol a <= srcLocStartCol b
       )
    && srcLocEndLine a > srcLocEndLine b
    || (srcLocEndLine a == srcLocEndLine b && srcLocEndCol a >= srcLocEndCol b)
