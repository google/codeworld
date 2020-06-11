{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
module CodeWorld.Color where

import Control.DeepSeq
import GHC.Generics (Generic)

data Color
  = RGBA
      !Double
      !Double
      !Double
      !Double
  deriving (Generic, Show, Eq)

instance NFData Color

type Colour = Color

pattern RGB :: Double -> Double -> Double -> Color
pattern RGB r g b = RGBA r g b 1

pattern HSL :: Double -> Double -> Double -> Color
pattern HSL h s l <-
  (toHSL -> Just (h, s, l))
  where
    HSL h s l = fromHSL h s l

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

mixed :: [Color] -> Color
mixed colors = go 0 0 0 0 0 colors
  where
    go rr gg bb aa n ((fenceColor -> RGBA r g b a) : cs) =
      go (rr + r * r * a) (gg + g * g * a) (bb + b * b * a) (aa + a) (n + 1) cs
    go rr gg bb aa n []
      | aa == 0 = RGBA 0 0 0 0
      | otherwise = RGBA (sqrt (rr / aa)) (sqrt (gg / aa)) (sqrt (bb / aa)) (aa / n)

-- Helper function that sets the alpha of the second color to that
-- of the first
sameAlpha :: Color -> Color -> Color
sameAlpha (fenceColor -> RGBA _ _ _ a1) (fenceColor -> RGBA r2 g2 b2 _) =
  RGBA r2 g2 b2 a1

lighter :: Double -> Color -> Color
lighter d c =
  sameAlpha c $ HSL (hue c) (saturation c) (fence (luminosity c + d))

light :: Color -> Color
light = lighter 0.15

darker :: Double -> Color -> Color
darker d = lighter (- d)

dark :: Color -> Color
dark = darker 0.15

brighter :: Double -> Color -> Color
brighter d c =
  sameAlpha c $ HSL (hue c) (fence (saturation c + d)) (luminosity c)

bright :: Color -> Color
bright = brighter 0.25

duller :: Double -> Color -> Color
duller d = brighter (- d)

dull :: Color -> Color
dull = duller 0.25

translucent :: Color -> Color
translucent (fenceColor -> RGBA r g b a) = RGBA r g b (a / 2)

-- | An infinite list of colors.
assortedColors :: [Color]
assortedColors = [HSL (adjusted h) 0.75 0.5 | h <- [0, 2 * pi / phi ..]]
  where
    phi = (1 + sqrt 5) / 2
    adjusted x =
      x + a0
        + a1 * sin (1 * x)
        + b1 * cos (1 * x)
        + a2 * sin (2 * x)
        + b2 * cos (2 * x)
        + a3 * sin (3 * x)
        + b3 * cos (3 * x)
        + a4 * sin (4 * x)
        + b4 * cos (4 * x)
    a0 = -8.6870353473225553e-02
    a1 = 8.6485747604766350e-02
    b1 = -9.6564816819163041e-02
    a2 = -3.0072759267059756e-03
    b2 = 1.5048456422494966e-01
    a3 = 9.3179137558373148e-02
    b3 = 2.9002513227535595e-03
    a4 = -6.6275768228887290e-03
    b4 = -1.0451841243520298e-02

hue :: Color -> Double
hue (fenceColor -> RGBA r g b _)
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
saturation (fenceColor -> RGBA r g b _)
  | hi - lo < epsilon = 0
  | otherwise = (hi - lo) / (1 - abs (hi + lo - 1))
  where
    hi = max r (max g b)
    lo = min r (min g b)
    epsilon = 0.000001

luminosity :: Color -> Double
luminosity (fenceColor -> RGBA r g b _) = (lo + hi) / 2
  where
    hi = max r (max g b)
    lo = min r (min g b)

alpha :: Color -> Double
alpha (RGBA _ _ _ a) = fence a

-- Old-style colors

white, black, red, green, blue, cyan, magenta, yellow :: Color

orange, rose, chartreuse, aquamarine, violet, azure :: Color

gray, grey, brown, purple, pink :: Color

white = HSL 0.00 0.00 1.00

black = HSL 0.00 0.00 0.00

gray = HSL 0.00 0.00 0.50

grey = HSL 0.00 0.00 0.50

red = HSL 0.00 0.75 0.50

orange = HSL 0.61 0.75 0.50

yellow = HSL 0.98 0.75 0.50

green = HSL 2.09 0.75 0.50

blue = HSL 3.84 0.75 0.50

purple = HSL 4.80 0.75 0.50

pink = HSL 5.76 0.75 0.75

brown = HSL 0.52 0.60 0.40

cyan = HSL (3 / 3 * pi) 0.75 0.5

magenta = HSL (5 / 3 * pi) 0.75 0.5

chartreuse = HSL (3 / 6 * pi) 0.75 0.5

aquamarine = HSL (5 / 6 * pi) 0.75 0.5

azure = HSL (7 / 6 * pi) 0.75 0.5

violet = HSL (9 / 6 * pi) 0.75 0.5

rose = HSL (11 / 6 * pi) 0.75 0.5

{-# WARNING
  magenta
  [ "Please use HSL(5 * pi / 3, 0.75, 0.5) instead of magenta.",
    "The variable magenta may be removed July 2020."
  ]
  #-}

{-# WARNING
  cyan
  [ "Please use HSL(pi, 0.75, 0.5) instead of cyan.",
    "The variable cyan may be removed July 2020."
  ]
  #-}

{-# WARNING
  chartreuse
  [ "Please use HSL(pi / 2, 0.75, 0.5) instead of chartreuse.",
    "The variable chartreuse may be removed July 2020."
  ]
  #-}

{-# WARNING
  aquamarine
  [ "Please use HSL(5 * pi / 6, 0.75, 0.5) instead of aquamarine.",
    "The variable aquamarine may be removed July 2020."
  ]
  #-}

{-# WARNING
  azure
  [ "Please use HSL(7 * pi / 6, 0.75, 0.5) instead of azure.",
    "The variable azure may be removed July 2020."
  ]
  #-}

{-# WARNING
  rose
  [ "Please use HSL(11 * pi / 6, 0.75, 0.5) instead of rose.",
    "The variable rose may be removed July 2020."
  ]
  #-}

{-# WARNING
  violet
  [ "Please use purple instead of violet.",
    "The variable violet may be removed July 2020."
  ]
  #-}
