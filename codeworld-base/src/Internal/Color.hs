{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

{-
  Copyright 2014 Google Inc. All rights reserved.

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

white, black :: Color
white = RGBA (1, 1, 1, 1)
black = RGBA (0, 0, 0, 1)

-- Primary and secondary colors
red, green, blue, cyan, magenta, yellow :: Color
red        = RGBA (1, 0, 0, 1)
green      = RGBA (0, 1, 0, 1)
blue       = RGBA (0, 0, 1, 1)
yellow     = RGBA (1, 1, 0, 1)
cyan       = RGBA (0, 1, 1, 1)
magenta    = RGBA (1, 0, 1, 1)

-- Tertiary colors
orange, rose, chartreuse, aquamarine, violet, azure :: Color
orange     = RGBA (1.0, 0.5, 0.0, 1)
rose       = RGBA (1.0, 0.0, 0.5, 1)
chartreuse = RGBA (0.5, 1.0, 0.0, 1)
aquamarine = RGBA (0.0, 1.0, 0.5, 1)
violet     = RGBA (0.5, 0.0, 1.0, 1)
azure      = RGBA (0.0, 0.5, 1.0, 1)

-- Other common colors and color names
brown  = RGBA (0.7, 0.4, 0.3, 1)
purple = violet
pink   = light rose

light :: Color -> Color
light (RGBA (r, g, b, a)) = RGBA (
    min (1, r + 0.4),
    min (1, g + 0.4),
    min (1, b + 0.4),
    a)

dark :: Color -> Color
dark (RGBA (r, g, b, a)) = RGBA (
    max (0, r - 0.4),
    max (0, g - 0.4),
    max (0, b - 0.4),
    a)

translucent :: Color -> Color
translucent (RGBA (r, g, b, a)) = RGBA (r, g, b, a/2)

bright :: Color -> Color
bright (RGBA (r, g, b, a)) = RGBA (
    min (1, max (0, (5*r - b - g) / 3)),
    min (1, max (0, (5*g - r - b) / 3)),
    min (1, max (0, (5*b - g - r) / 3)),
    a)

muted :: Color -> Color
muted (RGBA (r, g, b, a)) = RGBA (
    (r + avg) / 2,
    (g + avg) / 2,
    (b + avg) / 2,
    a)
  where avg = (r + g + b) / 3

gray, grey :: Number -> Color
gray = grey
grey k = RGBA (k, k, k, 1)
