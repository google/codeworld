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

mixed :: [Color] -> Color
mixed = fromCWColor . CW.mixed . P.map toCWColor

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

assortedColors :: [Color]
assortedColors = P.map fromCWColor CW.assortedColors

hue, saturation, luminosity, alpha :: Color -> Number
hue = (180 *) . (/ pi) . fromDouble . CW.hue . toCWColor

saturation = fromDouble . CW.saturation . toCWColor

luminosity = fromDouble . CW.luminosity . toCWColor

alpha = fromDouble . CW.alpha . toCWColor

-- New style colors

pattern White :: Color
pattern White  = HSL(  0, 0.00, 1.00)

pattern Black :: Color
pattern Black  = HSL(  0, 0.00, 0.00)

pattern Gray :: Color
pattern Gray   = HSL(  0, 0.00, 0.50)

pattern Grey :: Color
pattern Grey   = HSL(  0, 0.00, 0.50)

pattern Red :: Color
pattern Red    = HSL(  0, 0.75, 0.50)

pattern Orange :: Color
pattern Orange = HSL( 35, 0.75, 0.50)

pattern Yellow :: Color
pattern Yellow = HSL( 56, 0.75, 0.50)

pattern Green :: Color
pattern Green  = HSL(120, 0.75, 0.50)

pattern Blue :: Color
pattern Blue   = HSL(220, 0.75, 0.50)

pattern Purple :: Color
pattern Purple = HSL(275, 0.75, 0.50)

pattern Pink :: Color
pattern Pink   = HSL(330, 0.75, 0.75)

pattern Brown :: Color
pattern Brown  = HSL( 30, 0.60, 0.40)

-- Old style colors

white, black, red, green, blue, cyan, magenta, yellow :: Color
orange, rose, chartreuse, aquamarine, violet, azure :: Color
gray, grey :: Number -> Color

white = fromCWColor CW.white
black = fromCWColor CW.black
red = fromCWColor CW.red
yellow = fromCWColor CW.yellow
green = fromCWColor CW.green
cyan = fromCWColor CW.cyan
blue = fromCWColor CW.blue
magenta = fromCWColor CW.magenta
orange = fromCWColor CW.orange
chartreuse = fromCWColor CW.chartreuse
aquamarine = fromCWColor CW.aquamarine
azure = fromCWColor CW.azure
violet = fromCWColor CW.violet
rose = fromCWColor CW.rose
brown = fromCWColor CW.brown
purple = fromCWColor CW.purple
pink = fromCWColor CW.pink
gray = fromCWColor . CW.gray . toDouble
grey = gray

{-# WARNING white      [ "Please use White (capitalized) instead of white."
                       , "The variable white may be removed July 2019." ] #-}
{-# WARNING black      [ "Please use Black (capitalized) instead of black."
                       , "The variable black may be removed July 2019." ] #-}
{-# WARNING red        [ "Please use Red (capitalized) instead of red."
                       , "The variable red may be removed July 2019." ] #-}
{-# WARNING green      [ "Please use Green (capitalized) instead of green."
                       , "The variable green may be removed July 2019." ] #-}
{-# WARNING blue       [ "Please use Blue (capitalized) instead of blue."
                       , "The variable blue may be removed July 2019." ] #-}
{-# WARNING yellow     [ "Please use Yellow (capitalized) instead of yellow."
                       , "The variable yellow may be removed July 2019." ] #-}
{-# WARNING orange     [ "Please use Orange (capitalized) instead of orange."
                       , "The variable orange may be removed July 2019." ] #-}
{-# WARNING brown      [ "Please use Brown (capitalized) instead of brown."
                       , "The variable brown may be removed July 2019." ] #-}
{-# WARNING purple     [ "Please use Purple (capitalized) instead of purple."
                       , "The variable purple may be removed July 2019." ] #-}
{-# WARNING pink       [ "Please use Pink (capitalized) instead of pink."
                       , "The variable pink may be removed July 2019." ] #-}
{-# WARNING magenta    [ "Please use RGB(...) instead of magenta."
                       , "The variable magenta may be removed July 2019." ] #-}
{-# WARNING cyan       [ "Please use RGB(...) instead of cyan."
                       , "The variable cyan may be removed July 2019." ] #-}
{-# WARNING chartreuse [ "Please use RGB(...) instead of chartreuse."
                       , "The variable chartreuse may be removed July 2019." ] #-}
{-# WARNING aquamarine [ "Please use RGB(...) instead of aquamarine."
                       , "The variable aquamarine may be removed July 2019." ] #-}
{-# WARNING azure      [ "Please use RGB(...) instead of azure."
                       , "The variable azure may be removed July 2019." ] #-}
{-# WARNING rose       [ "Please use RGB(...) instead of rose."
                       , "The variable rose may be removed July 2019." ] #-}
{-# WARNING violet     [ "Please use Purple instead of violet."
                       , "The variable violet may be removed July 2019." ] #-}
{-# WARNING gray       [ "Please use Gray with the light/dark functions instead."
                       , "The gray(...) function may be removed July 2019." ] #-}
{-# WARNING grey       [ "Please use Grey with the light/dark functions instead."
                       , "The grey(...) function may be removed July 2019." ] #-}
