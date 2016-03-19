{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

{-
  Copyright 2016 The CodeWorld Authors. All rights reserved.

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
import                           Internal.Num
import                           Internal.Text
import                           Internal.Color
import           "base"          Prelude (map, (.))

type Point = (Number, Number)
type Vector = (Number, Number)

toCWVect   :: Vector -> CW.Vector
toCWVect (x, y) = (toDouble x, toDouble y)

fromCWVect :: CW.Vector -> Vector
fromCWVect (x, y) = (fromDouble x, fromDouble y)

vectorSum :: (Vector, Vector) -> Vector
vectorSum (v, w) = fromCWVect (CW.vectorSum (toCWVect v) (toCWVect w))

vectorDifference :: (Vector, Vector) -> Vector
vectorDifference (v, w) = fromCWVect (CW.vectorDifference (toCWVect v) (toCWVect w))

scaledVector :: (Vector, Number) -> Vector
scaledVector (v, k) = fromCWVect (CW.scaledVector (toDouble k) (toCWVect v))

rotatedVector :: (Vector, Number) -> Vector
rotatedVector (v, k) = fromCWVect (CW.rotatedVector (toDouble (pi * k / 180)) (toCWVect v))

dotProduct :: (Vector, Vector) -> Number
dotProduct (v, w) = fromDouble (CW.dotProduct (toCWVect v) (toCWVect w))

newtype Picture = CWPic { toCWPic :: CW.Picture }
data Font = Serif | SansSerif | Monospace | Handwriting | Fancy | NamedFont !Text
data TextStyle = Plain | Italic | Bold

-- | A blank picture
blank :: Picture
blank = CWPic CW.blank

-- | A thin sequence of line segments with these endpoints
path :: [Point] -> Picture
path = CWPic . CW.path . map toCWVect

-- | A thin sequence of line segments, with these endpoints and line width
thickPath :: ([Point], Number) -> Picture
thickPath (ps, n) = CWPic (CW.thickPath (toDouble n) (map toCWVect ps))

-- | A thin line with these points as endpoints
line :: [Point] -> Picture
line = CWPic . CW.line . map toCWVect

-- | A thick line, with these endpoints, with this line width
thickLine :: ([Point], Number) -> Picture
thickLine (ps, n) = CWPic (CW.thickLine (toDouble n) (map toCWVect ps))

-- | A thin polygon with these points as vertices
polygon :: [Point] -> Picture
polygon = CWPic . CW.polygon . map toCWVect

-- | A thin polygon with these points as vertices
thickPolygon :: ([Point], Number) -> Picture
thickPolygon (ps, n) = CWPic (CW.thickPolygon (toDouble n) (map toCWVect ps))

-- | A solid polygon with these points as vertices
solidPolygon :: [Point] -> Picture
solidPolygon = CWPic . CW.solidPolygon . map toCWVect

-- | A thin rectangle, with this width and height
rectangle :: (Number, Number) -> Picture
rectangle (w, h) = CWPic (CW.rectangle (toDouble w) (toDouble h))

-- | A solid rectangle, with this width and height
solidRectangle :: (Number, Number) -> Picture
solidRectangle (w, h) = CWPic (CW.solidRectangle (toDouble w) (toDouble h))

-- | A thick rectangle, with this width and height and line width
thickRectangle :: (Number, Number, Number) -> Picture
thickRectangle (w, h, lw) = CWPic
    (CW.thickRectangle (toDouble lw) (toDouble w) (toDouble h))

-- | A thin circle, with this radius
circle :: Number -> Picture
circle = CWPic . CW.circle . toDouble

-- | A solid circle, with this radius
solidCircle :: Number -> Picture
solidCircle = CWPic . CW.solidCircle . toDouble

-- | A thick circle, with this radius and line width
thickCircle :: (Number, Number) -> Picture
thickCircle (r, w) = CWPic (CW.thickCircle (toDouble w) (toDouble r))

-- | A thin arc, starting and ending at these angles, with this radius
arc :: (Number, Number, Number) -> Picture
arc (b, e, r) = CWPic
    (CW.arc (toDouble (pi * b / 180)) (toDouble (pi * e / 180)) (toDouble r))

-- | A solid sector of a circle (i.e., a pie slice) starting and ending at these
-- angles, with this radius
sector :: (Number, Number, Number) -> Picture
sector (b, e, r) = CWPic
    (CW.sector (toDouble (pi * b / 180)) (toDouble (pi * e / 180)) (toDouble r))

-- | A thick arc, starting and ending at these angles, with this radius and
-- line width
thickArc :: (Number, Number, Number, Number) -> Picture
thickArc (b, e, r, w) = CWPic
    (CW.thickArc (toDouble w)
                 (toDouble (pi * b / 180))
                 (toDouble (pi * e / 180))
                 (toDouble r))

-- | A piece of text
text :: Text -> Picture
text = CWPic . CW.text . fromCWText

-- | A styled piece of text
styledText :: (Text, Font, TextStyle) -> Picture
styledText (t, f, s) = CWPic
    (CW.styledText (fromCWStyle s) (fromCWFont f) (fromCWText t))
  where fromCWStyle Plain           = CW.Plain
        fromCWStyle Bold            = CW.Bold
        fromCWStyle Italic          = CW.Italic
        fromCWFont  Serif           = CW.Serif
        fromCWFont  SansSerif       = CW.SansSerif
        fromCWFont  Monospace       = CW.Monospace
        fromCWFont  Handwriting     = CW.Handwriting
        fromCWFont  Fancy           = CW.Fancy
        fromCWFont  (NamedFont fnt) = CW.NamedFont (fromCWText fnt)

-- | A picture drawn entirely in this color.
colored :: (Picture, Color) -> Picture
colored (p, c) = CWPic (CW.colored (toCWColor c) (toCWPic p))

-- | A picture drawn entirely in this color.
coloured :: (Picture, Color) -> Picture
coloured = colored

-- | A picture drawn translated in these directions.
translated :: (Picture, Number, Number) -> Picture
translated (p, x, y) = CWPic (CW.translated (toDouble x) (toDouble y) (toCWPic p))

-- | A picture scaled by these factors.
scaled :: (Picture, Number, Number) -> Picture
scaled (p, x, y) = CWPic (CW.scaled (toDouble x) (toDouble y) (toCWPic p))

-- | A picture scaled by these factors.
dilated :: (Picture, Number, Number) -> Picture
dilated = scaled

-- | A picture rotated by this angle.
rotated :: (Picture, Number) -> Picture
rotated (p, th) = CWPic (CW.rotated (toDouble (pi * th / 180)) (toCWPic p))

-- A picture made by drawing these pictures, ordered from top to bottom.
pictures :: [Picture] -> Picture
pictures = CWPic . CW.pictures . map toCWPic

-- Binary composition of pictures.
(&) :: Picture -> Picture -> Picture
infixr 0 &
a & b = CWPic (toCWPic a CW.& toCWPic b)

-- | A coordinate plane.  Adding this to your pictures can help you measure distances
-- more accurately.
--
-- Example:
--
--    main = pictureOf(myPicture & coordinatePlane)
--    myPicture = ...
coordinatePlane :: Picture
coordinatePlane = CWPic CW.coordinatePlane

-- | The CodeWorld logo.
codeWorldLogo :: Picture
codeWorldLogo = CWPic CW.codeWorldLogo
