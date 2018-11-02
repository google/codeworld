{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

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
module CodeWorld.Color where

import Data.Fixed (mod')
import Data.List (unfoldr)

import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

data Color =
    RGBA !Double
         !Double
         !Double
         !Double
    deriving (Show, Eq)

type Colour = Color

pattern RGB :: Double -> Double -> Double -> Color

pattern RGB r g b = RGBA r g b 1

pattern HSL :: Double -> Double -> Double -> Color

pattern HSL h s l <- (toHSL -> Just (h, s, l))
  where HSL h s l = fromHSL h s l

-- Utility functions for pattern synonyms.
fence :: Double -> Double
fence = max 0 . min 1

wrapNum :: Double -> Double -> Double
wrapNum lim x = x - fromInteger (floor (x / lim)) * lim

fenceColor :: Color -> Color
fenceColor (RGBA r g b a) = RGBA (fence r) (fence g) (fence b) (fence a)

-- Based on the algorithm from the CSS3 specification.
fromHSL :: Double -> Double -> Double -> Color
fromHSL (wrapNum (2 * pi) -> h) (fence -> s) (fence -> l) = RGBA r g b 1
  where
    m1 = l * 2 - m2
    m2
        | l <= 0.5 = l * (s + 1)
        | otherwise = l + s - l * s
    r = convert m1 m2 (h / 2 / pi + 1 / 3)
    g = convert m1 m2 (h / 2 / pi)
    b = convert m1 m2 (h / 2 / pi - 1 / 3)
    convert m1 m2 h
        | h < 0 = convert m1 m2 (h + 1)
        | h > 1 = convert m1 m2 (h - 1)
        | h * 6 < 1 = m1 + (m2 - m1) * h * 6
        | h * 2 < 1 = m2
        | h * 3 < 2 = m1 + (m2 - m1) * (2 / 3 - h) * 6
        | otherwise = m1

toHSL :: Color -> Maybe (Double, Double, Double)
toHSL c@(RGBA _ _ _ 1) = Just (hue c, saturation c, luminosity c)
toHSL _ = Nothing

white, black :: Color
white = RGBA 1 1 1 1

black = RGBA 0 0 0 1

-- Primary and secondary colors
red, green, blue, cyan, magenta, yellow :: Color
red = HSL (0 / 3 * pi) 0.75 0.5

yellow = HSL (1 / 3 * pi) 0.75 0.5

green = HSL (2 / 3 * pi) 0.75 0.5

cyan = HSL (3 / 3 * pi) 0.75 0.5

blue = HSL (4 / 3 * pi) 0.75 0.5

magenta = HSL (5 / 3 * pi) 0.75 0.5

-- Tertiary colors
orange, rose, chartreuse, aquamarine, violet, azure :: Color
orange = HSL (1 / 6 * pi) 0.75 0.5

chartreuse = HSL (3 / 6 * pi) 0.75 0.5

aquamarine = HSL (5 / 6 * pi) 0.75 0.5

azure = HSL (7 / 6 * pi) 0.75 0.5

violet = HSL (9 / 6 * pi) 0.75 0.5

rose = HSL (11 / 6 * pi) 0.75 0.5

-- Other common colors and color names
brown = HSL (1 / 6 * pi) 0.5 0.5

purple = violet

pink = lighter 0.25 rose

mixed :: Color -> Color -> Color
mixed (fenceColor -> RGBA r1 g1 b1 a1) (fenceColor -> RGBA r2 g2 b2 a2)
    | a1 + a2 == 0 = RGBA 0 0 0 0
    | otherwise = RGBA r g b a
  where
    r = sqrt ((r1 ^ 2 * a1 + r2 ^ 2 * a2) / (a1 + a2))
    g = sqrt ((g1 ^ 2 * a1 + g2 ^ 2 * a2) / (a1 + a2))
    b = sqrt ((b1 ^ 2 * a1 + b2 ^ 2 * a2) / (a1 + a2))
    a = (a1 + a2) / 2

{-# WARNING mixed
    ["The type of mixed will soon be changed to: mixed :: [Color] -> Color",
     "All existing uses will break, but are easily fixed."] #-}

-- Helper function that sets the alpha of the second color to that
-- of the first
sameAlpha :: Color -> Color -> Color
sameAlpha (fenceColor -> RGBA r1 g1 b1 a1) (fenceColor -> RGBA r2 g2 b2 a2) =
    RGBA r2 g2 b2 a1

lighter :: Double -> Color -> Color
lighter d c =
    sameAlpha c $ HSL (hue c) (saturation c) (fence (luminosity c + d))

light :: Color -> Color
light = lighter 0.15

darker :: Double -> Color -> Color
darker d = lighter (-d)

dark :: Color -> Color
dark = darker 0.15

brighter :: Double -> Color -> Color
brighter d c =
    sameAlpha c $ HSL (hue c) (fence (saturation c + d)) (luminosity c)

bright :: Color -> Color
bright = brighter 0.25

duller :: Double -> Color -> Color
duller d = brighter (-d)

dull :: Color -> Color
dull = duller 0.25

translucent :: Color -> Color
translucent (fenceColor -> RGBA r g b a) = RGBA r g b (a / 2)

gray, grey :: Double -> Color
gray = grey

grey (fence -> k) = RGBA k k k 1

{-# WARNING gray
    ["The function argument to gray will soon be removed.",
     "All existing uses will break, but are easily fixed.",
     "To avoid breaking, consider replacing (gray k) with (HSL 0 0 k)."] #-}

{-# WARNING grey
    ["The function argument to grey will soon be removed.",
     "All existing uses will break, but are easily fixed.",
     "To avoid breaking, consider replacing (grey k) with (HSL 0 0 k)."] #-}

-- | An infinite list of colors.
assortedColors :: [Color]
assortedColors = [ HSL (adjusted h) 0.75 0.5 | h <- [0, 2 * pi / phi ..] ]
  where
    phi = (1 + sqrt 5) / 2
    adjusted x = x + a0
               + a1 * sin (1*x) + b1 * cos (1*x)
               + a2 * sin (2*x) + b2 * cos (2*x)
               + a3 * sin (3*x) + b3 * cos (3*x)
               + a4 * sin (4*x) + b4 * cos (4*x)
    a0 = -8.6870353473225553e-02
    a1 =  8.6485747604766350e-02
    b1 = -9.6564816819163041e-02
    a2 = -3.0072759267059756e-03
    b2 =  1.5048456422494966e-01
    a3 =  9.3179137558373148e-02
    b3 =  2.9002513227535595e-03
    a4 = -6.6275768228887290e-03
    b4 = -1.0451841243520298e-02

hue :: Color -> Double
hue (fenceColor -> RGBA r g b a)
    | hi - lo < epsilon = 0
    | r == hi && g >= b = (g - b) / (hi - lo) * pi / 3
    | r == hi = (g - b) / (hi - lo) * pi / 3 + 2 * pi
    | g == hi = (b - r) / (hi - lo) * pi / 3 + 2 / 3 * pi
    | otherwise = (r - g) / (hi - lo) * pi / 3 + 4 / 3 * pi
  where
    hi = max r (max g b)
    lo = min r (min g b)
    epsilon = 0.000001

saturation :: Color -> Double
saturation (fenceColor -> RGBA r g b a)
    | hi - lo < epsilon = 0
    | otherwise = (hi - lo) / (1 - abs (hi + lo - 1))
  where
    hi = max r (max g b)
    lo = min r (min g b)
    epsilon = 0.000001

luminosity :: Color -> Double
luminosity (fenceColor -> RGBA r g b a) = (lo + hi) / 2
  where
    hi = max r (max g b)
    lo = min r (min g b)

alpha :: Color -> Double
alpha (RGBA r g b a) = fence a
