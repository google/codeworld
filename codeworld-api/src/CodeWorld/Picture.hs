{-# LANGUAGE CPP #-}

{-
  Copyright 2018 The CodeWorld Authors. All rights reserved.

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
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Stack

type Point = (Double, Double)

translatedPoint :: Double -> Double -> Point -> Point
translatedPoint tx ty (x, y) = (x + tx, y + ty)

rotatedPoint :: Double -> Point -> Point
rotatedPoint = rotatedVector

scaledPoint :: Double -> Double -> Point -> Point
scaledPoint kx ky (x, y) = (kx * x, ky * y)

dilatedPoint :: Double -> Point -> Point
dilatedPoint k (x, y) = (k * x, k * y)

type Vector = (Double, Double)

vectorLength :: Vector -> Double
vectorLength (x, y) = sqrt (x ^ 2 + y ^ 2)

vectorDirection :: Vector -> Double
vectorDirection (x, y) = atan2 y x

vectorSum :: Vector -> Vector -> Vector
vectorSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vectorDifference :: Vector -> Vector -> Vector
vectorDifference (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

scaledVector :: Double -> Vector -> Vector
scaledVector k (x, y) = (k * x, k * y)

{-| Angle is in radians -}
rotatedVector :: Double -> Vector -> Vector
rotatedVector angle (x, y) =
    (x * cos angle - y * sin angle, x * sin angle + y * cos angle)

dotProduct :: Vector -> Vector -> Double
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

data Picture
    = SolidPolygon CallStack
              [Point]
    | SolidClosedCurve CallStack
              [Point]
    | Polygon CallStack
           [Point]
    | ThickPolygon CallStack
           [Point]
           !Double
    | Rectangle CallStack
           !Double
           !Double
    | SolidRectangle CallStack
           !Double
           !Double
    | ThickRectangle CallStack
           !Double
           !Double
           !Double
    | ClosedCurve CallStack
           [Point]
    | ThickClosedCurve CallStack
           [Point]
           !Double
    | Polyline CallStack
           [Point]
    | ThickPolyline CallStack
           [Point]
           !Double
    | Curve CallStack
           [Point]
    | ThickCurve CallStack
           [Point]
           !Double
    | Circle CallStack
           !Double
    | SolidCircle CallStack
           !Double
    | ThickCircle CallStack
           !Double
           !Double
    | Sector CallStack
             !Double
             !Double
             !Double
    | Arc CallStack
          !Double
          !Double
          !Double
    | ThickArc CallStack
          !Double
          !Double
          !Double
          !Double
    | StyledText CallStack
           !TextStyle
           !Font
           !Text
    | Text CallStack
           !Text
    | Color CallStack
            !Color
            !Picture
    | Translate CallStack
                !Double
                !Double
                !Picture
    | Scale CallStack
            !Double
            !Double
            !Picture
    | Dilate CallStack
             !Double
             !Picture
    | Rotate CallStack
             !Double
             !Picture
    | CoordinatePlane CallStack
    | Logo CallStack
    | Pictures [Picture]
    | Blank CallStack

data TextStyle
    = Plain
    | Bold
    | Italic deriving (Show)

data Font
    = SansSerif
    | Serif
    | Monospace
    | Handwriting
    | Fancy
    | NamedFont !Text deriving (Show)

-- | A blank picture
blank :: HasCallStack => Picture
blank = Blank callStack

-- | A thin sequence of line segments, with these points as endpoints
polyline :: HasCallStack => [Point] -> Picture
polyline ps = Polyline callStack ps

-- | A thin sequence of line segments, with these points as endpoints
path :: HasCallStack => [Point] -> Picture
path ps = Polyline callStack ps

{-# WARNING path ["Please use polyline instead of path.",
                  "path may be removed July 2019."] #-}

-- | A thick sequence of line segments, with given line width and endpoints
thickPolyline :: HasCallStack => Double -> [Point] -> Picture
thickPolyline n ps = ThickPolyline callStack ps n

-- | A thick sequence of line segments, with given line width and endpoints
thickPath :: HasCallStack => Double -> [Point] -> Picture
thickPath n ps = ThickPolyline callStack ps n

{-# WARNING thickPath ["Please used thickPolyline instead of thickPath.",
                       "thickPath may be removed July 2019."] #-}

-- | A thin polygon with these points as vertices
polygon :: HasCallStack => [Point] -> Picture
polygon ps = Polygon callStack ps

-- | A thick polygon with this line width and these points as
-- vertices
thickPolygon :: HasCallStack => Double -> [Point] -> Picture
thickPolygon n ps = ThickPolygon callStack ps n

-- | A solid polygon with these points as vertices
solidPolygon :: HasCallStack => [Point] -> Picture
solidPolygon ps = SolidPolygon callStack ps

-- | A smooth curve passing through these points.
curve :: HasCallStack => [Point] -> Picture
curve ps = Curve callStack ps

-- | A thick smooth curve with this line width, passing through these points.
thickCurve :: HasCallStack => Double -> [Point] -> Picture
thickCurve n ps = ThickCurve callStack ps n

-- | A smooth closed curve passing through these points.
closedCurve :: HasCallStack => [Point] -> Picture
closedCurve ps = ClosedCurve callStack ps

-- | A thick smooth closed curve with this line width, passing through these points.
thickClosedCurve :: HasCallStack => Double -> [Point] -> Picture
thickClosedCurve n ps = ThickClosedCurve callStack ps n

-- | A solid smooth closed curve passing through these points.
solidClosedCurve :: HasCallStack => [Point] -> Picture
solidClosedCurve ps = SolidClosedCurve callStack ps

rectangleVertices :: Double -> Double -> [Point]
rectangleVertices w h = [ (w / 2, h / 2), (w / 2, -h / 2), (-w / 2, -h / 2), (-w / 2, h / 2) ]

-- | A thin rectangle, with this width and height
rectangle :: HasCallStack => Double -> Double -> Picture
rectangle w h = Rectangle callStack w h

-- | A solid rectangle, with this width and height
solidRectangle :: HasCallStack => Double -> Double -> Picture
solidRectangle w h = SolidRectangle callStack w h

-- | A thick rectangle, with this line width, and width and height
thickRectangle :: HasCallStack => Double -> Double -> Double -> Picture
thickRectangle lw w h = ThickRectangle callStack lw w h

-- | A thin circle, with this radius
circle :: HasCallStack => Double -> Picture
circle = Circle callStack 

-- | A thick circle, with this line width and radius
thickCircle :: HasCallStack => Double -> Double -> Picture
thickCircle = ThickCircle callStack

-- | A thin arc, starting and ending at these angles, with this radius
--
-- Angles are in radians.
arc :: HasCallStack => Double -> Double -> Double -> Picture
arc b e r = Arc callStack b e r

-- | A thick arc with this line width, starting and ending at these angles,
-- with this radius.
--
-- Angles are in radians.
thickArc :: HasCallStack => Double -> Double -> Double -> Double -> Picture
thickArc w b e r = ThickArc callStack b e r w

-- | A solid circle, with this radius
solidCircle :: HasCallStack => Double -> Picture
solidCircle = SolidCircle callStack 

-- | A solid sector of a circle (i.e., a pie slice) starting and ending at these
-- angles, with this radius
--
-- Angles are in radians.
sector :: HasCallStack => Double -> Double -> Double -> Picture
sector = Sector callStack

-- | A rendering of text characters.
text :: HasCallStack => Text -> Picture
text = Text callStack

-- | A rendering of text characters.
lettering :: HasCallStack => Text -> Picture
lettering = Text callStack

-- | A rendering of text characters, with a specific choice of font and style.
styledText :: HasCallStack => TextStyle -> Font -> Text -> Picture
styledText = StyledText callStack

-- | A rendering of text characters onto a Picture, with a specific
-- choice of font and style.
styledLettering :: HasCallStack => TextStyle -> Font -> Text -> Picture
styledLettering = StyledText callStack

-- | A picture drawn entirely in this color.
colored :: HasCallStack => Color -> Picture -> Picture
colored = Color callStack

-- | A picture drawn entirely in this colour.
coloured :: HasCallStack => Color -> Picture -> Picture
coloured = colored

-- | A picture drawn translated in these directions.
translated :: HasCallStack => Double -> Double -> Picture -> Picture
translated = Translate callStack

-- | A picture scaled by these factors.
scaled :: HasCallStack => Double -> Double -> Picture -> Picture
scaled = Scale callStack

-- | A picture scaled by these factors.
dilated :: HasCallStack => Double -> Picture -> Picture
dilated = Dilate callStack

-- | A picture rotated by this angle.
--
-- Angles are in radians.
rotated :: HasCallStack => Double -> Picture -> Picture
rotated = Rotate callStack

-- A picture made by drawing these pictures, ordered from top to bottom.
pictures :: [Picture] -> Picture
pictures = Pictures

#if MIN_VERSION_base(4,11,0)

instance Semigroup Picture where
    a <> (Pictures bs) = Pictures (a : bs)
    a <> b             = Pictures [a, b]

#endif

instance Monoid Picture where
    mempty = blank
    mappend a (Pictures bs) = Pictures (a : bs)
    mappend a b = Pictures [a, b]
    mconcat = pictures

-- | Binary composition of pictures.
(&) :: Picture -> Picture -> Picture
infixr 0 &

(&) = mappend

-- | A coordinate plane.  Adding this to your pictures can help you measure distances
-- more accurately.
--
-- Example:
--
--    main = drawingOf (myPicture <> coordinatePlane)
--    myPicture = ...
coordinatePlane :: HasCallStack => Picture
coordinatePlane = CoordinatePlane callStack

-- | The CodeWorld logo.
codeWorldLogo :: HasCallStack => Picture
codeWorldLogo = Logo callStack
