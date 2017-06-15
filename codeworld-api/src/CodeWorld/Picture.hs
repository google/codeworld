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

data Picture = Polygon [Point] !Bool
             | Path [Point] !Double !Bool !Bool
             | Sector !Double !Double !Double
             | Arc !Double !Double !Double !Double
             | Text !TextStyle !Font !Text
             | Color !Color !Picture
             | Translate !Double !Double !Picture
             | Scale !Double !Double !Picture
             | Rotate !Double !Picture
             | Pictures [Picture]
             | Logo

data TextStyle = Plain | Bold | Italic

data Font = SansSerif | Serif | Monospace | Handwriting | Fancy | NamedFont !Text

-- | A blank picture
blank :: Picture
blank = Pictures []

-- | A thin sequence of line segments, with these points as endpoints
path :: [Point] -> Picture
path ps = Path ps 0 False False

-- | A thick sequence of line segments, with given line width and endpoints
thickPath :: Double -> [Point] -> Picture
thickPath n ps = Path ps n False False

-- | A thin polygon with these points as vertices
polygon :: [Point] -> Picture
polygon ps = Path ps 0 True False

-- | A thick polygon with this line width and these points as
-- vertices
thickPolygon :: Double -> [Point] -> Picture
thickPolygon n ps = Path ps n True False

-- | A solid polygon with these points as vertices
solidPolygon :: [Point] -> Picture
solidPolygon ps = Polygon ps False

-- | A smooth curve passing through these points.
curve :: [Point] -> Picture
curve ps = Path ps 0 False True

-- | A thick smooth curve with this line width, passing through these points.
thickCurve :: Double -> [Point] -> Picture
thickCurve n ps = Path ps n False True

-- | A smooth closed loop passing through these points.
loop :: [Point] -> Picture
loop ps = Path ps 0 True True

-- | A thick smooth closed loop with this line width, passing through these points.
thickLoop :: Double -> [Point] -> Picture
thickLoop n ps = Path ps n True True

-- | A solid smooth closed loop passing through these points.
solidLoop :: [Point] -> Picture
solidLoop ps = Polygon ps True

-- | A thin rectangle, with this width and height
rectangle :: Double -> Double -> Picture
rectangle w h = polygon [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)
    ]

-- | A solid rectangle, with this width and height
solidRectangle :: Double -> Double -> Picture
solidRectangle w h = solidPolygon [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)
    ]

-- | A thick rectangle, with this line width, and width and height
thickRectangle :: Double -> Double -> Double -> Picture
thickRectangle lw w h = thickPolygon lw [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)
    ]

-- | A thin circle, with this radius
circle :: Double -> Picture
circle = arc 0 (2*pi)

-- | A thick circle, with this line width and radius
thickCircle :: Double -> Double -> Picture
thickCircle w = thickArc w 0 (2*pi)

-- | A thin arc, starting and ending at these angles, with this radius
--
-- Angles are in radians.
arc :: Double -> Double -> Double -> Picture
arc b e r = Arc b e r 0

-- | A thick arc with this line width, starting and ending at these angles,
-- with this radius.
--
-- Angles are in radians.
thickArc :: Double -> Double -> Double -> Double -> Picture
thickArc w b e r = Arc b e r w

-- | A solid circle, with this radius
solidCircle :: Double -> Picture
solidCircle = sector 0 (2*pi)

-- | A solid sector of a circle (i.e., a pie slice) starting and ending at these
-- angles, with this radius
--
-- Angles are in radians.
sector :: Double -> Double -> Double -> Picture
sector = Sector

-- | A piece of text
text :: Text -> Picture
text = Text Plain Serif

styledText :: TextStyle -> Font -> Text -> Picture
styledText = Text

-- | A picture drawn entirely in this color.
colored :: Color -> Picture -> Picture
colored = Color

-- | A picture drawn entirely in this colour.
coloured :: Color -> Picture -> Picture
coloured = colored

-- | A picture drawn translated in these directions.
translated :: Double -> Double -> Picture -> Picture
translated = Translate

-- | A picture scaled by these factors.
scaled :: Double -> Double -> Picture -> Picture
scaled = Scale

-- | A picture scaled by these factors.
dilated :: Double -> Double -> Picture -> Picture
dilated = scaled

-- | A picture rotated by this angle.
--
-- Angles are in radians.
rotated :: Double -> Picture -> Picture
rotated = Rotate

-- A picture made by drawing these pictures, ordered from top to bottom.
pictures :: [Picture] -> Picture
pictures = Pictures

instance Monoid Picture where
  mempty                   = blank
  mappend a (Pictures bs)  = Pictures (a:bs)
  mappend a b              = Pictures [a, b]
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
codeWorldLogo :: Picture
codeWorldLogo = Logo
