{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

{-
  Copyright 2015 Google Inc. All rights reserved.

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

module Internal.Color where

import qualified "base" Prelude as P
import Internal.Num

newtype Color = RGBA(Number, Number, Number, Number)
instance P.Show Color where show _ = "<<Color>>"

type Colour = Color

white, black :: Color
white = RGBA (1, 1, 1, 1)
black = RGBA (0, 0, 0, 1)

-- Primary and secondary colors
red, green, blue, cyan, magenta, yellow :: Color
red        = fromHSL (  0, 0.75, 0.5)
yellow     = fromHSL ( 60, 0.75, 0.5)
green      = fromHSL (120, 0.75, 0.5)
cyan       = fromHSL (180, 0.75, 0.5)
blue       = fromHSL (240, 0.75, 0.5)
magenta    = fromHSL (300, 0.75, 0.5)

-- Tertiary colors
orange, rose, chartreuse, aquamarine, violet, azure :: Color
orange     = fromHSL ( 30, 0.75, 0.5)
chartreuse = fromHSL ( 90, 0.75, 0.5)
aquamarine = fromHSL (150, 0.75, 0.5)
azure      = fromHSL (210, 0.75, 0.5)
violet     = fromHSL (270, 0.75, 0.5)
rose       = fromHSL (330, 0.75, 0.5)

-- Other common colors and color names
brown      = fromHSL ( 15, 0.5,  0.5)
purple     = fromHSL (280, 0.75, 0.5)
pink       = fromHSL (345, 0.75, 0.75)

mixColors :: (Color, Color) -> Color
mixColors (RGBA (r1,g1,b1,a1), RGBA (r2, g2, b2, a2))
  | a1 + a2 P.== 0 = RGBA (0, 0, 0, 0)
  | P.otherwise    = RGBA (r, g, b, a)
  where r = sqrt(r1^2 * a1 + r2^2 * a2 / (a1 + a2))
        g = sqrt(g1^2 * a1 + g2^2 * a2 / (a1 + a2))
        b = sqrt(b1^2 * a1 + b2^2 * a2 / (a1 + a2))
        a = (a1 + a2) / 2
{-# WARNING mixColors "Please use mixOfColors(...) instead of mixColors(...)" #-}

mixOfColors :: (Color, Color) -> Color
mixOfColors = mixColors

lighter :: (Color, Number) -> Color
lighter (c, d) = fromHSL (hue c, saturation c, fence (luminosity c + d))
  where fence x = max (0, min (1, x))

light :: Color -> Color
light c = lighter (c, 0.15)

darker :: (Color, Number) -> Color
darker (c, d) = lighter (c, -d)

dark :: Color -> Color
dark c = darker (c, 0.15)

brighter :: (Color, Number) -> Color
brighter (c, d) = fromHSL (hue c, fence (saturation c + d), luminosity c)
  where fence x = max (0, min (1, x))

bright :: Color -> Color
bright c = brighter (c, 0.25)

duller :: (Color, Number) -> Color
duller (c, d) = brighter (c, -d)

dull :: Color -> Color
dull c = duller (c, 0.25)

translucent :: Color -> Color
translucent (RGBA (r, g, b, a)) = RGBA (r, g, b, a/2)

gray, grey :: Number -> Color
gray = grey
grey k = RGBA (k, k, k, 1)

hue (RGBA (r, g, b, a))
  | hi P.== lo            = 0
  | r P.== hi P.&& g >= b = 60 * (g - b) / (hi - lo)
  | r P.== hi             = 60 * (g - b) / (hi - lo) + 360
  | g P.== hi             = 60 * (b - r) / (hi - lo) + 120
  | P.otherwise           = 60 * (r - g) / (hi - lo) + 240
  where hi = max (r, max (g, b))
        lo = min (r, min (g, b))

saturation (RGBA (r, g, b, a))
  | hi P.== lo  = 0
  | P.otherwise = (hi - lo) / (1 - abs (hi + lo - 1))
  where hi = max (r, max (g, b))
        lo = min (r, min (g, b))

luminosity (RGBA (r, g, b, a)) = (lo + hi) / 2
  where hi = max (r, max (g, b))
        lo = min (r, min (g, b))

fromHSL (h, s, l)
  | h < 0       = fromHSL (h + 360, s, l)
  | h > 360     = fromHSL (h - 360, s, l)
  | h < 60      = RGBA (c + m, x + m, m,     1)
  | h < 120     = RGBA (x + m, c + m, m,     1)
  | h < 180     = RGBA (m,     c + m, x + m, 1)
  | h < 240     = RGBA (m,     x + m, c + m, 1)
  | h < 300     = RGBA (x + m, m,     c + m, 1)
  | P.otherwise = RGBA (c + m, m,     x + m, 1)
  where c                      = (1 - abs (2 * l - 1)) * s
        hnorm                  = h / 60
        x | even (floor hnorm) = c * (1 - abs (hnorm - floor hnorm - 1))
          | P.otherwise        = c * (1 - abs (hnorm - floor hnorm))
        m                      = l - c / 2
