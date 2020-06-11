{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
module Internal.Color where

import qualified "codeworld-api" CodeWorld as CW
import Internal.Num
import Internal.Truth
import qualified "base" Prelude as P
import "base" Prelude ((.))

-- | A color.
--
-- Colors can be described in several ways:
--
-- 1. Using common color names: 'red', 'blue', 'yellow', 'green',
--    'orange', 'purple', 'pink', 'black', 'white', 'brown', and
--    'gray'.
-- 2. Transforming other colors with functions such as 'light',
--    'dark', 'bright', 'dull', 'translucent', and 'mixed'.
-- 3. Constructing colors from coordinates in a color space, such
--    as 'RGB', 'RGBA', or 'HSL'.
--
-- Note that transparency is included in a color.  Common color
-- names and the 'RGB' and 'HSL' constructors only produce opaque
-- colors, but 'RGBA' and the 'translucent' function work with
-- transparency.
newtype Color = Color {toCWColor :: CW.Color} deriving (P.Eq)

-- | A synonym for 'Color', using the non-US spelling.
type Colour = Color

{-# RULES
"equality/color" forall (x :: Color). (==) x = (P.==) x
  #-}

pattern RGBA :: (Number, Number, Number, Number) -> Color
pattern RGBA components <-
  (toRGBA -> components)
  where
    RGBA components =
      let (r, g, b, a) = components
       in Color
            ( CW.RGBA
                (toDouble r)
                (toDouble g)
                (toDouble b)
                (toDouble a)
            )

-- Utility function for RGB pattern synonym.
toRGBA :: Color -> (Number, Number, Number, Number)
toRGBA (Color (CW.RGBA r g b a)) =
  (fromDouble r, fromDouble g, fromDouble b, fromDouble a)

pattern RGB :: (Number, Number, Number) -> Color
pattern RGB components <-
  (toRGB -> P.Just components)
  where
    RGB components =
      let (r, g, b) = components
       in Color
            ( CW.RGBA
                (toDouble r)
                (toDouble g)
                (toDouble b)
                (toDouble 1)
            )

-- Utility function for RGB pattern synonym.
toRGB :: Color -> P.Maybe (Number, Number, Number)
toRGB (Color (CW.RGBA r g b (fromDouble -> 1))) =
  P.Just (fromDouble r, fromDouble g, fromDouble b)
toRGB _ = P.Nothing

pattern HSL :: (Number, Number, Number) -> Color
pattern HSL components <-
  (toHSL -> P.Just components)
  where
    HSL components = fromHSL components

-- Utility functions for HSL pattern synonym.
toHSL :: Color -> P.Maybe (Number, Number, Number)
toHSL c@(RGBA (_, _, _, 1)) = P.Just (hue c, saturation c, luminosity c)
toHSL _ = P.Nothing

fromHSL :: (Number, Number, Number) -> Color
fromHSL (h, s, l) =
  Color (CW.HSL (toDouble (pi * h / 180)) (toDouble s) (toDouble l))

-- | Produces a color by mixing other colors in equal proportion.
--
-- The order of colors is unimportant.  Colors may be mixed in uneven
-- proportions by listing a color more than once, such as
-- @mixed([red, red, orange])@.
mixed :: [Color] -> Color
mixed = Color . CW.mixed . P.map toCWColor

-- | Increases the luminosity of a color by the given amount.
--
-- The amount should be between -1 and 1, where:
--
-- * @lighter(c, 1)@ is always white, regardless of @c@.
-- * @lighter(c, 0)@ is the same as @c@.
-- * @lighter(c, -1)@ is always black, regardless of @c@.
lighter :: (Color, Number) -> Color
lighter (c, d) = Color (CW.lighter (toDouble d) (toCWColor c))

-- | Produces a lighter shade of the given color.
--
-- This function may be nested more than once to produce an even
-- lighter shade, as in @light(light(blue))@.
light :: Color -> Color
light = Color . CW.light . toCWColor

-- | Decreases the luminosity of a color by the given amount.
--
-- The amount should be between -1 and 1, where:
--
-- * @darker(c, 1)@ is always black, regardless of @c@.
-- * @darker(c, 0)@ is the same as @c@.
-- * @darker(c, -1)@ is always white, regardless of @c@.
darker :: (Color, Number) -> Color
darker (c, d) = Color (CW.darker (toDouble d) (toCWColor c))

-- | Produces a darker shade of the given color.
--
-- This function may be nested more than once to produce an even
-- darker shade, as in @dark(dark(green))@.
dark :: Color -> Color
dark = Color . CW.dark . toCWColor

-- | Increases the saturation of a color by the given amount.
--
-- The amount should be between -1 and 1, where:
--
-- * @brighter(c, 1)@ is a fully saturated version of @c@.
-- * @brighter(c, 0)@ is the same as @c@.
-- * @brighter(c, -1)@ is just a shade of gray with no color.
brighter :: (Color, Number) -> Color
brighter (c, d) = Color (CW.brighter (toDouble d) (toCWColor c))

-- | Produces a brighter shade of the given color; that is, less
-- gray and more colorful.
--
-- This function may be nested more than once to produce an even
-- brighter shade, as in @bright(bright(yellow))@.
bright :: Color -> Color
bright = Color . CW.bright . toCWColor

-- | Decreases the saturation of a color by the given amount.
--
-- The amount should be between -1 and 1, where:
--
-- * @duller(c, 1)@ is just a shade of gray with no color.
-- * @duller(c, 0)@ is the same as @c@.
-- * @duller(c, -1)@ is a fully saturated version of @c@.
duller :: (Color, Number) -> Color
duller (c, d) = Color (CW.duller (toDouble d) (toCWColor c))

-- | Produces a duller shade of the given color; that is, more
-- gray and less colorful.
--
-- This function may be nested more than once to produce an even
-- duller shade, as in @dull(dull(purple))@.
dull :: Color -> Color
dull = Color . CW.dull . toCWColor

-- | Produces a partially transparent color.
--
-- This function may be nested more than once to produce an even
-- more transparent color, as in @translucent(translucent(brown))@.
translucent :: Color -> Color
translucent = Color . CW.translucent . toCWColor

-- | An infinite list of various colors.
--
-- The list is chosen to contain a variety of different hues as
-- spread out as possible to create colorful effects.
assortedColors :: [Color]
assortedColors = P.map Color CW.assortedColors

hue, saturation, luminosity, alpha :: Color -> Number
hue = (180 *) . (/ pi) . fromDouble . CW.hue . toCWColor
saturation = fromDouble . CW.saturation . toCWColor
luminosity = fromDouble . CW.luminosity . toCWColor
alpha = fromDouble . CW.alpha . toCWColor

-- New style colors

-- | The color white
white :: Color
white = Color CW.white

-- | The color black
black :: Color
black = Color CW.black

-- | The color gray
gray :: Color
gray = Color CW.gray

-- | The color grey
--
-- This is the same color as 'gray', but with a non-US
-- spelling.
grey :: Color
grey = Color CW.grey

-- | The color red
red :: Color
red = Color CW.red

-- | The color orange
orange :: Color
orange = Color CW.orange

-- | The color yellow
yellow :: Color
yellow = Color CW.yellow

-- | The color green
green :: Color
green = Color CW.green

-- | The color blue
blue :: Color
blue = Color CW.blue

-- | The color purple
purple :: Color
purple = Color CW.purple

-- | The color pink
pink :: Color
pink = Color CW.pink

-- | The color brown
brown :: Color
brown = Color CW.brown

cyan, magenta, rose, chartreuse, aquamarine, violet, azure :: Color
cyan = Color CW.cyan
magenta = Color CW.magenta
rose = Color CW.rose
chartreuse = Color CW.chartreuse
aquamarine = Color CW.aquamarine
violet = Color CW.violet
azure = Color CW.azure
{-# WARNING
  magenta
  [ "Please use HSL(300, 0.75, 0.5) instead of magenta.",
    "The variable magenta may be removed July 2020."
  ]
  #-}
{-# WARNING
  cyan
  [ "Please use HSL(180, 0.75, 0.5) instead of cyan.",
    "The variable cyan may be removed July 2020."
  ]
  #-}
{-# WARNING
  chartreuse
  [ "Please use HSL(90, 0.75, 0.5) instead of chartreuse.",
    "The variable chartreuse may be removed July 2020."
  ]
  #-}
{-# WARNING
  aquamarine
  [ "Please use HSL(150, 0.75, 0.5) instead of aquamarine.",
    "The variable aquamarine may be removed July 2020."
  ]
  #-}
{-# WARNING
  azure
  [ "Please use HSL(210, 0.75, 0.5) instead of azure.",
    "The variable azure may be removed July 2020."
  ]
  #-}
{-# WARNING
  rose
  [ "Please use HSL(330, 0.75, 0.5) instead of rose.",
    "The variable rose may be removed July 2020."
  ]
  #-}
{-# WARNING
  violet
  [ "Please use purple instead of violet.",
    "The variable violet may be removed July 2020."
  ]
  #-}

{-# WARNING
  hue
  [ "Please match HSL(...) instead of using hue(...).",
    "The hue function may be removed July 2020."
  ]
  #-}

{-# WARNING
  saturation
  [ "Please match HSL(...) instead of using saturation(...).",
    "The saturation function may be removed July 2020."
  ]
  #-}

{-# WARNING
  luminosity
  [ "Please match HSL(...) instead of using luminosity(...).",
    "The luminosity function may be removed July 2020."
  ]
  #-}

{-# WARNING
  alpha
  [ "Please match RGBA(...) instead of using alpha(...).",
    "The alpha function may be removed July 2020."
  ]
  #-}
