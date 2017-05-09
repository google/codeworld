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

module CodeWorld.Color where

import Data.Functor (fmap)
import Data.List (unfoldr)
import Data.Fixed (mod')

data Color = RGBA !Double !Double !Double !Double deriving (Show, Eq)
type Colour = Color

white, black :: Color
white = RGBA 1 1 1 1
black = RGBA 0 0 0 1

-- Primary and secondary colors
red, green, blue, cyan, magenta, yellow :: Color
red        = fromHSL (0/3 * pi) 0.75 0.5
yellow     = fromHSL (1/3 * pi) 0.75 0.5
green      = fromHSL (2/3 * pi) 0.75 0.5
cyan       = fromHSL (3/3 * pi) 0.75 0.5
blue       = fromHSL (4/3 * pi) 0.75 0.5
magenta    = fromHSL (5/3 * pi) 0.75 0.5

-- Tertiary colors
orange, rose, chartreuse, aquamarine, violet, azure :: Color
orange     = fromHSL ( 1/6 * pi) 0.75 0.5
chartreuse = fromHSL ( 3/6 * pi) 0.75 0.5
aquamarine = fromHSL ( 5/6 * pi) 0.75 0.5
azure      = fromHSL ( 7/6 * pi) 0.75 0.5
violet     = fromHSL ( 9/6 * pi) 0.75 0.5
rose       = fromHSL (11/6 * pi) 0.75 0.5

-- Other common colors and color names
brown      = fromHSL (1/6   * pi) 0.5  0.5
purple     = fromHSL (1.556 * pi) 0.75 0.5
pink       = fromHSL (23/12 * pi) 0.75 0.75

mixed :: Color -> Color -> Color
mixed (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2)
  | a1 + a2 == 0 = RGBA 0 0 0 0
  | otherwise    = RGBA r g b a
  where r = sqrt(r1^2 * a1 + r2^2 * a2 / (a1 + a2))
        g = sqrt(g1^2 * a1 + g2^2 * a2 / (a1 + a2))
        b = sqrt(b1^2 * a1 + b2^2 * a2 / (a1 + a2))
        a = (a1 + a2) / 2

-- Helper function that sets the alpha of the second color to that
-- of the first
sameAlpha :: Color -> Color -> Color
sameAlpha (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) = RGBA r2 g2 b2 a1

lighter :: Double -> Color -> Color
lighter d c = sameAlpha c $ fromHSL (hue c) (saturation c) (fence (luminosity c + d))
  where fence x = max 0 (min 1 x)

light :: Color -> Color
light = lighter 0.15

darker :: Double -> Color -> Color
darker d = lighter (-d)

dark :: Color -> Color
dark = darker 0.15

brighter :: Double -> Color -> Color
brighter d c = sameAlpha c $ fromHSL (hue c) (fence (saturation c + d)) (luminosity c)
  where fence x = max 0 (min 1 x)

bright :: Color -> Color
bright = brighter 0.25

duller :: Double -> Color -> Color
duller d = brighter (-d)

dull :: Color -> Color
dull = duller 0.25

translucent :: Color -> Color
translucent (RGBA r g b a) = RGBA r g b (a/2)

gray, grey :: Double -> Color
gray = grey
grey k = RGBA k k k 1


-- Produce an infinite list of colors:
-- https://github.com/google/codeworld/issues/436
-- The list loops through the HSL spectrum with fixed
-- saturation and lightness:
-- https://www.w3schools.com/colors/colors_hsl.asp
-- http://stackoverflow.com/a/7728626/1007926

colorHues :: [Double]
colorHues = let
  start = 0.0
  end = 2*pi
  foldF :: (Double, Double) -> Maybe (Double, (Double, Double))
  foldF (level, index) = let
    pow2 = 2.0 ** (level - 1.0)
    result = (end - start) * index / pow2
    index' = mod' (index + pi/8) pow2
    level' = if (index' == 1.0) then (level+1) else level
    in Just (result, (level', index'))
  in unfoldr foldF (1.0, 1.0)

colors :: [Color]
colors = fmap (\h -> fromHSL h 0.75 0.5) colorHues

hue :: Color -> Double
hue (RGBA r g b a)
  | hi == lo           = 0
  | r  == hi && g >= b = (g - b) / (hi - lo) * pi / 3
  | r  == hi           = (g - b) / (hi - lo) * pi / 3 + 2 * pi
  | g  == hi           = (b - r) / (hi - lo) * pi / 3 + 2/3 * pi
  | otherwise          = (r - g) / (hi - lo) * pi / 3 + 4/3 * pi
  where hi = max r (max g b)
        lo = min r (min g b)

saturation :: Color -> Double
saturation (RGBA r g b a)
  | hi == lo  = 0
  | otherwise = (hi - lo) / (1 - abs (hi + lo - 1))
  where hi = max r (max g b)
        lo = min r (min g b)

luminosity :: Color -> Double
luminosity (RGBA r g b a) = (lo + hi) / 2
  where hi = max r (max g b)
        lo = min r (min g b)

-- Based on the algorithm from the CSS3 specification.
fromHSL :: Double -> Double -> Double -> Color
fromHSL h s l = RGBA r g b 1
  where m1             = l * 2 - m2
        m2 | l <= 0.5  = l * (s + 1)
           | otherwise = l + s - l * s
        r              = convert m1 m2 (h / 2 / pi + 1/3)
        g              = convert m1 m2 (h / 2 / pi      )
        b              = convert m1 m2 (h / 2 / pi - 1/3)
        convert m1 m2 h
          | h < 0     = convert m1 m2 (h + 1)
          | h > 1     = convert m1 m2 (h - 1)
          | h * 6 < 1 = m1 + (m2 - m1) * h * 6
          | h * 2 < 1 = m2
          | h * 3 < 2 = m1 + (m2 - m1) * (2/3 - h) * 6
          | otherwise = m1
