{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
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
module Internal.Color where

import qualified "codeworld-api" CodeWorld as CW
import Internal.Num
import Internal.Truth
import qualified "base" Prelude as P
import "base" Prelude ((.))

newtype Color =
    RGBA (Number, Number, Number, Number)
    deriving (P.Eq)

type Colour = Color

{-# RULES
"equality/color" forall (x :: Color) . (==) x = (P.==) x
 #-}

pattern RGB :: (Number, Number, Number) -> Color

pattern RGB components <- (toRGB -> P.Just components)
  where RGB components
          = let (r, g, b) = components in RGBA (r, g, b, 1)

-- Utility function for RGB pattern synonym.
toRGB :: Color -> P.Maybe (Number, Number, Number)
toRGB (RGBA (r, g, b, 1)) = P.Just (r, g, b)
toRGB _ = P.Nothing

pattern HSL :: (Number, Number, Number) -> Color

pattern HSL components <- (toHSL -> P.Just components)
  where HSL components = fromHSL components

-- Utility functions for HSL pattern synonym.
toHSL :: Color -> P.Maybe (Number, Number, Number)
toHSL c@(RGBA (_, _, _, 1)) = P.Just (hue c, saturation c, luminosity c)
toHSL _ = P.Nothing

fromHSL :: (Number, Number, Number) -> Color
fromHSL (h, s, l) =
    fromCWColor (CW.HSL (toDouble (pi * h / 180)) (toDouble s) (toDouble l))

toCWColor :: Color -> CW.Color
toCWColor (RGBA (r, g, b, a)) =
    CW.RGBA (toDouble r) (toDouble g) (toDouble b) (toDouble a)

fromCWColor :: CW.Color -> Color
fromCWColor (CW.RGBA r g b a) =
    RGBA (fromDouble r, fromDouble g, fromDouble b, fromDouble a)

white, black :: Color
white = fromCWColor CW.white

black = fromCWColor CW.black

-- Primary and secondary colors
red, green, blue, cyan, magenta, yellow :: Color
red = fromCWColor CW.red

yellow = fromCWColor CW.yellow

green = fromCWColor CW.green

cyan = fromCWColor CW.cyan

blue = fromCWColor CW.blue

magenta = fromCWColor CW.magenta

-- Tertiary colors
orange, rose, chartreuse, aquamarine, violet, azure :: Color
orange = fromCWColor CW.orange

chartreuse = fromCWColor CW.chartreuse

aquamarine = fromCWColor CW.aquamarine

azure = fromCWColor CW.azure

violet = fromCWColor CW.violet

rose = fromCWColor CW.rose

-- Other common colors and color names
brown = fromCWColor CW.brown

purple = fromCWColor CW.purple

pink = fromCWColor CW.pink

mixed :: (Color, Color) -> Color
mixed (a, b) = fromCWColor (CW.mixed (toCWColor a) (toCWColor b))

{-# WARNING mixed
    ["The type of mixed will soon be changed to: mixed :: [Color] -> Color",
     "All existing uses will break, but are easily fixed."] #-}

lighter :: (Color, Number) -> Color
lighter (c, d) = fromCWColor (CW.lighter (toDouble d) (toCWColor c))

light :: Color -> Color
light = fromCWColor . CW.light . toCWColor

darker :: (Color, Number) -> Color
darker (c, d) = fromCWColor (CW.darker (toDouble d) (toCWColor c))

dark :: Color -> Color
dark = fromCWColor . CW.dark . toCWColor

brighter :: (Color, Number) -> Color
brighter (c, d) = fromCWColor (CW.brighter (toDouble d) (toCWColor c))

bright :: Color -> Color
bright = fromCWColor . CW.bright . toCWColor

duller :: (Color, Number) -> Color
duller (c, d) = fromCWColor (CW.duller (toDouble d) (toCWColor c))

dull :: Color -> Color
dull = fromCWColor . CW.dull . toCWColor

translucent :: Color -> Color
translucent = fromCWColor . CW.translucent . toCWColor

gray, grey :: Number -> Color
gray = fromCWColor . CW.gray . toDouble

grey = gray

assortedColors :: [Color]
assortedColors = P.map fromCWColor CW.assortedColors

hue, saturation, luminosity, alpha :: Color -> Number
hue = (180 *) . (/ pi) . fromDouble . CW.hue . toCWColor

saturation = fromDouble . CW.saturation . toCWColor

luminosity = fromDouble . CW.luminosity . toCWColor

alpha = fromDouble . CW.alpha . toCWColor

-- New style colors

pattern White :: Color
pattern Black :: Color
pattern Gray :: Color
pattern Grey :: Color
pattern Red :: Color
pattern Orange :: Color
pattern Yellow :: Color
pattern Green :: Color
pattern Blue :: Color
pattern Purple :: Color
pattern Pink :: Color
pattern Brown :: Color

pattern White  = HSL(  0, 0.00, 1.00)
pattern Black  = HSL(  0, 0.00, 0.00)
pattern Gray   = HSL(  0, 0.00, 0.50)
pattern Grey   = HSL(  0, 0.00, 0.50)
pattern Red    = HSL(  0, 0.75, 0.50)
pattern Orange = HSL( 35, 0.75, 0.50)
pattern Yellow = HSL( 56, 0.75, 0.50)
pattern Green  = HSL(120, 0.75, 0.50)
pattern Blue   = HSL(220, 0.75, 0.50)
pattern Purple = HSL(275, 0.75, 0.50)
pattern Pink   = HSL(330, 0.75, 0.75)
pattern Brown  = HSL( 30, 0.60, 0.40)

{-# WARNING White "This is an experimental API." #-}
{-# WARNING Black "This is an experimental API." #-}
{-# WARNING Gray "This is an experimental API." #-}
{-# WARNING Grey "This is an experimental API." #-}
{-# WARNING Red "This is an experimental API." #-}
{-# WARNING Orange "This is an experimental API." #-}
{-# WARNING Yellow "This is an experimental API." #-}
{-# WARNING Green "This is an experimental API." #-}
{-# WARNING Blue "This is an experimental API." #-}
{-# WARNING Purple "This is an experimental API." #-}
{-# WARNING Pink "This is an experimental API." #-}
{-# WARNING Brown "This is an experimental API." #-}
