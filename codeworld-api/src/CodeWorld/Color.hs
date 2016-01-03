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

module CodeWorld.Color where

data Color = RGBA !Double !Double !Double !Double deriving (Show, Eq)
type Colour = Color

white, black :: Color
white = RGBA 1 1 1 1
black = RGBA 0 0 0 1

-- Primary and secondary colors
red, green, blue, cyan, magenta, yellow :: Color
red        = fromHSL   0 0.75 0.5
yellow     = fromHSL  60 0.75 0.5
green      = fromHSL 120 0.75 0.5
cyan       = fromHSL 180 0.75 0.5
blue       = fromHSL 240 0.75 0.5
magenta    = fromHSL 300 0.75 0.5

-- Tertiary colors
orange, rose, chartreuse, aquamarine, violet, azure :: Color
orange     = fromHSL  30 0.75 0.5
chartreuse = fromHSL  90 0.75 0.5
aquamarine = fromHSL 150 0.75 0.5
azure      = fromHSL 210 0.75 0.5
violet     = fromHSL 270 0.75 0.5
rose       = fromHSL 330 0.75 0.5

-- Other common colors and color names
brown      = fromHSL  15 0.5  0.5
purple     = fromHSL 280 0.75 0.5
pink       = fromHSL 345 0.75 0.75

mixed :: Color -> Color -> Color
mixed (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2)
  | a1 + a2 == 0 = RGBA 0 0 0 0
  | otherwise    = RGBA r g b a
  where r = sqrt(r1^2 * a1 + r2^2 * a2 / (a1 + a2))
        g = sqrt(g1^2 * a1 + g2^2 * a2 / (a1 + a2))
        b = sqrt(b1^2 * a1 + b2^2 * a2 / (a1 + a2))
        a = (a1 + a2) / 2

lighter :: Double -> Color -> Color
lighter d c = fromHSL (hue c) (saturation c) (fence (luminosity c + d))
  where fence x = max 0 (min 1 x)

light :: Color -> Color
light = lighter 0.15

darker :: Double -> Color -> Color
darker d = lighter (-d)

dark :: Color -> Color
dark = darker 0.15

brighter :: Double -> Color -> Color
brighter d c = fromHSL (hue c) (fence (saturation c + d)) (luminosity c)
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

hue (RGBA r g b a)
  | hi == lo           = 0
  | r  == hi && g >= b = 60 * (g - b) / (hi - lo)
  | r  == hi           = 60 * (g - b) / (hi - lo) + 360
  | g  == hi           = 60 * (b - r) / (hi - lo) + 120
  | otherwise          = 60 * (r - g) / (hi - lo) + 240
  where hi = max r (max g b)
        lo = min r (min g b)

saturation (RGBA r g b a)
  | hi == lo  = 0
  | otherwise = (hi - lo) / (1 - abs (hi + lo - 1))
  where hi = max r (max g b)
        lo = min r (min g b)

luminosity (RGBA r g b a) = (lo + hi) / 2
  where hi = max r (max g b)
        lo = min r (min g b)

fromHSL h s l
  | h < 0     = fromHSL (h + 360) s l
  | h > 360   = fromHSL (h - 360) s l
  | h < 60    = RGBA (c + m) (x + m) (m    ) 1
  | h < 120   = RGBA (y + m) (c + m) (m    ) 1
  | h < 180   = RGBA (m    ) (c + m) (x + m) 1
  | h < 240   = RGBA (m    ) (y + m) (c + m) 1
  | h < 300   = RGBA (x + m) (m    ) (c + m) 1
  | otherwise = RGBA (c + m) (m    ) (y + m) 1
  where c     = (1 - abs (2 * l - 1)) * s
        (hnorm, rem) = properFraction (h / 60)
        x     = c * (1 - abs (rem - 1))
        y     = c * (1 - abs rem)
        m     = l - c / 2
