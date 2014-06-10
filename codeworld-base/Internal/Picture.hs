{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module Internal.Picture where

import qualified "base" Prelude as P
import Internal.Num
import Internal.Text
import Internal.Color

type Point = (Number, Number)

type Vector = (Number, Number)

addVectors :: Vector -> Vector -> Vector
addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

scaleVector :: Number -> Vector -> Vector
scaleVector k (x,y) = (k*x, k*y)

rotateVector :: Number -> Vector -> Vector
rotateVector angle (x,y) = (x * cos angle - y * sin angle,
                            x * sin angle + y * cos angle)

data Picture = Polygon [Point]
             | Line [Point] Number
             | Arc Number Number Number Number
             | Text Text
             | Color Color Picture
             | Translate Number Number Picture
             | Scale Number Number Picture
             | Rotate Number Picture
             | Pictures [Picture]

instance P.Show Picture where show _ = "<<Picture>>"

-- A blank picture
blank :: Picture
blank = Pictures []

-- A thin line with these points as endpoints
line :: [Point] -> Picture
line ps = Line ps 0

-- A solid polygon with these points as vertices
polygon :: [Point] -> Picture
polygon = Polygon

-- A thick line, with these endpoints, with this line width
thickLine :: [Point] -> Number -> Picture
thickLine = Line

-- A thin rectangle, with this width and height
rectangle :: Number -> Number -> Picture
rectangle w h = line [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2), (-w/2, -h/2)
    ]

-- A solid rectangle, with this width and height
solidRectangle :: Number -> Number -> Picture
solidRectangle w h = polygon [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2)
    ]

-- A thick rectangle, with this width and height and line width
thickRectangle :: Number -> Number -> Number -> Picture
thickRectangle w h lw = thickLine [
    (-w/2, -h/2), (w/2, -h/2), (w/2, h/2), (-w/2, h/2), (-w/2, -h/2)
    ] lw

-- A thin circle, with this radius
circle :: Number -> Picture
circle = arc 0 360

-- A solid circle, with this radius
solidCircle :: Number -> Picture
solidCircle r = thickCircle (r/2) r

-- A thick circle, with this radius and line width
thickCircle :: Number -> Number -> Picture
thickCircle = Arc 0 360

-- A thin arc, starting and ending at these angles, with this radius
arc :: Number -> Number -> Number -> Picture
arc b e r = Arc b e r 0

-- A solid sector of a circle (i.e., a pie slice) starting and ending at these
-- angles, with this radius
sector :: Number -> Number -> Number -> Picture
sector b e r = Arc b e (r/2) r

-- A thick arc, starting and ending at these angles, with this radius and
-- line width
thickArc :: Number -> Number -> Number -> Number -> Picture
thickArc = Arc

-- A piece of text
text :: Text -> Picture
text = Text

-- A picture drawn entirely in this color.
color :: Color -> Picture -> Picture
color = Color

-- A picture drawn translated in these directions.
translate :: Number -> Number -> Picture -> Picture
translate = Translate

-- A picture scaled by these factors.
scale :: Number -> Number -> Picture -> Picture
scale = Scale

-- A picture scaled by this angle.
rotate :: Number -> Picture -> Picture
rotate = Rotate

-- A picture made by drawing these pictures, ordered from top to bottom.
pictures :: [Picture] -> Picture
pictures = Pictures

-- Binary composition of pictures.
(&) :: Picture -> Picture -> Picture
a & b = Pictures [a, b]
