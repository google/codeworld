{-
  Copyright 2017 The CodeWorld Authors. All rights reserved.

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
type Vector = (Double, Double)

vectorLength :: Vector -> Double
vectorLength (x,y) = sqrt (x^2 + y^2)

vectorDirection :: Vector -> Double
vectorDirection (x,y) = atan2 y x

vectorSum :: Vector -> Vector -> Vector
vectorSum (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

vectorDifference :: Vector -> Vector -> Vector
vectorDifference (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)

scaledVector :: Double -> Vector -> Vector
scaledVector k (x,y) = (k*x, k*y)

{-| Angle is in radians -}
rotatedVector :: Double -> Vector -> Vector
rotatedVector angle (x,y) = (x * cos angle - y * sin angle,
                             x * sin angle + y * cos angle)

dotProduct :: Vector -> Vector -> Double
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

data Picture = Polygon CallStack [Point] !Bool
             | Path CallStack [Point] !Double !Bool !Bool
             | Sector CallStack !Double !Double !Double
             | Arc CallStack !Double !Double !Double !Double
             | Text CallStack !TextStyle !Font !Text
             | Color CallStack !Color !Picture
             | Translate CallStack !Double !Double !Picture
             | Scale CallStack !Double !Double !Picture
             | Rotate CallStack !Double !Picture
             | Pictures CallStack [Picture]
             | Logo CallStack

data TextStyle = Plain | Bold | Italic

data Font = SansSerif | Serif | Monospace | Handwriting | Fancy | NamedFont !Text

-- | A blank picture
blank :: HasCallStack => Picture
blank = Pictures callStack []

-- | A thin sequence of line segments, with these points as endpoints
path :: HasCallStack => [Point] -> Picture
path ps = Path callStack ps 0 False False

-- | A thick sequence of line segments, with given line width and endpoints
thickPath :: HasCallStack => Double -> [Point] -> Picture
thickPath n ps = Path callStack ps n False False

-- | A thin polygon with these points as vertices
polygon :: HasCallStack => [Point] -> Picture
polygon ps = Path callStack ps 0 True False

-- | A thick polygon with this line width and these points as
-- vertices
thickPolygon :: HasCallStack => Double -> [Point] -> Picture
thickPolygon n ps = Path callStack ps n True False

-- | A solid polygon with these points as vertices
solidPolygon :: HasCallStack => [Point] -> Picture
solidPolygon ps = Polygon callStack ps False

-- | A smooth curve passing through these points.
curve :: HasCallStack => [Point] -> Picture
curve ps = Path callStack ps 0 False True

-- | A thick smooth curve with this line width, passing through these points.
thickCurve :: HasCallStack => Double -> [Point] -> Picture
thickCurve n ps = Path callStack ps n False True

-- | A smooth closed loop passing through these points.
loop :: HasCallStack => [Point] -> Picture
loop ps = Path callStack ps 0 True True

-- | A thick smooth closed loop with this line width, passing through these points.
thickLoop :: HasCallStack => Double -> [Point] -> Picture
thickLoop n ps = Path callStack ps n True True

-- | A solid smooth closed loop passing through these points.
solidLoop :: HasCallStack => [Point] -> Picture
solidLoop ps = Polygon callStack ps True

-- | A thin rectangle, with this width and height
rectangle :: HasCallStack => Double -> Double -> Picture
rectangle w h = polygon [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)
    ]

-- | A solid rectangle, with this width and height
solidRectangle :: HasCallStack => Double -> Double -> Picture
solidRectangle w h = solidPolygon [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)
    ]

-- | A thick rectangle, with this line width, and width and height
thickRectangle :: HasCallStack => Double -> Double -> Double -> Picture
thickRectangle lw w h = thickPolygon lw [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)
    ]

-- | A thin circle, with this radius
circle :: HasCallStack => Double -> Picture
circle = arc 0 (2*pi)

-- | A thick circle, with this line width and radius
thickCircle :: HasCallStack => Double -> Double -> Picture
thickCircle w = thickArc w 0 (2*pi)

-- | A thin arc, starting and ending at these angles, with this radius
--
-- Angles are in radians.
arc :: HasCallStack => Double -> Double -> Double -> Picture
arc b e r = Arc callStack b e r 0

-- | A thick arc with this line width, starting and ending at these angles,
-- with this radius.
--
-- Angles are in radians.
thickArc :: HasCallStack => Double -> Double -> Double -> Double -> Picture
thickArc w b e r = Arc callStack b e r w

-- | A solid circle, with this radius
solidCircle :: HasCallStack => Double -> Picture
solidCircle = sector 0 (2*pi)

-- | A solid sector of a circle (i.e., a pie slice) starting and ending at these
-- angles, with this radius
--
-- Angles are in radians.
sector :: HasCallStack => Double -> Double -> Double -> Picture
sector = Sector callStack

-- | A piece of text
text :: HasCallStack => Text -> Picture
text = Text callStack Plain Serif

styledText :: HasCallStack => TextStyle -> Font -> Text -> Picture
styledText = Text callStack

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
dilated k = scaled k k

-- | A picture rotated by this angle.
--
-- Angles are in radians.
rotated :: HasCallStack => Double -> Picture -> Picture
rotated = Rotate callStack

-- A picture made by drawing these pictures, ordered from top to bottom.
pictures :: HasCallStack => [Picture] -> Picture
pictures = Pictures callStack

instance Monoid Picture where
  mempty                   = blank
  mappend a (Pictures _ bs)  = Pictures callStack (a:bs)
  mappend a b              = Pictures callStack [a, b]
  mconcat                  = pictures

-- | Binary composition of pictures.
(&) :: Picture -> Picture -> Picture
infixr 0 &
(&) = mappend

-- | A coordinate plane.  Adding this to your pictures can help you measure distances
-- more accurately.
--
-- Example:
--
--    main = pictureOf (myPicture <> coordinatePlane)
--    myPicture = ...
coordinatePlane :: Picture
coordinatePlane = axes <> numbers <> guidelines
  where xline y     = thickPath 0.01 [(-10, y), (10, y)]
        xaxis       = thickPath 0.03 [(-10, 0), (10, 0)]
        axes        = xaxis <> rotated (pi/2) xaxis
        xguidelines = pictures [ xline k | k <- [-10, -9 .. 10] ]
        guidelines  = xguidelines <> rotated (pi/2) xguidelines
        numbers = xnumbers <> ynumbers
        xnumbers = pictures
            [ translated (fromIntegral k) 0.3 (scaled 0.5 0.5 (text (pack (show k))))
              | k <- [-9, -8 .. 9], k /= 0 ]
        ynumbers = pictures
            [ translated 0.3 (fromIntegral k) (scaled 0.5 0.5 (text (pack (show k))))
              | k <- [-9, -8 .. 9], k /= 0 ]

-- | The CodeWorld logo.
codeWorldLogo :: HasCallStack => Picture
codeWorldLogo = Logo callStack
