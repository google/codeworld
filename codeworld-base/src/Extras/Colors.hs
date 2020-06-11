{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Convenience functions for specifying colors by name, hexadecimal value,
-- integer value between 0 and 255, and a few other ways.
module Extras.Colors
  ( -- $intro
    painted,
    colorNamed,
    rgb,
    greyed,
    withAlpha,
  )
where

import Prelude

-------------------------------------------------------------------------------

-- $intro
-- = Extra Colors
--
-- To use the extra features in this module, you must begin your code with this
-- line:
--
-- > import Extras.Colors

-- | The given picture painted with the color corresponding to the given
-- color name, which must be a valid argument of the 'colorNamed' function.
painted :: (Picture, Text) -> Picture
painted (pic, name) = colored (pic, colorNamed (name))

-- | This function allows you to specify color components in the range 0 to 255
-- instead of 0 to 1.
rgb :: (Number, Number, Number) -> Color
rgb (r, g, b) = RGB (r / 255, g / 255, b / 255)

-- | A shade of grey as given by the argument, where @greyed(0)@ is @black@
-- and @greyed(1)@ is white.
greyed :: Number -> Color
greyed (g) = RGB (g, g, g)

-- | This function allows you to specify the level of transparency of the
-- given color. Transparency must be given in the range 0 (fully transparent)
-- to 1 (fully opaque).
withAlpha :: (Color, Number) -> Color
withAlpha (RGBA (r, g, b, _), a) = RGBA (r, g, b, a)

-- | Convert the name of a color to the actual color. The name of the color
-- can be specified in two ways. First, you can use the official CSS color,
-- as defined in the working document /CSS Color Module Level 4/, which
-- can be found at <https://drafts.csswg.org/css-color/#named-colors>.
-- You can also specify the color as a Text with the pattern "#xxxxxx", where
-- the first character is a hash tag and the rest are 6 hexadecimal digits
-- that correspond to the /hex color/, as usually defined in HTML documents.
--
-- The list of named colors is:
--
-- @aliceblue@, @antiquewhite@, @aqua@, @aquamarine@, @azure@, @beige@,
-- @bisque@, @black@, @blanchedalmond@, @blue@, @blueviolet@, @brown@,
-- @burlywood@, @cadetblue@, @chartreuse@, @chocolate@, @coral@,
-- @cornflowerblue@, @cornsilk@, @crimson@, @cyan@, @darkblue@,
-- @darkcyan@, @darkgoldenrod@, @darkgray@, @darkgreen@, @darkgrey@,
-- @darkkhaki@, @darkmagenta@, @darkolivegreen@, @darkorange@,
-- @darkorchid@, @darkred@, @darksalmon@, @darkseagreen@, @darkslateblue@,
-- @darkslategray@, @darkslategrey@, @darkturquoise@, @darkviolet@,
-- @deeppink@, @deepskyblue@, @dimgray@, @dimgrey@, @dodgerblue@,
-- @firebrick@, @floralwhite@, @forestgreen@, @fuchsia@, @gainsboro@,
-- @ghostwhite@, @gold@, @goldenrod@, @gray@, @green@, @greenyellow@,
-- @grey@, @honeydew@, @hotpink@, @indianred@, @indigo@, @ivory@,
-- @khaki@, @lavender@, @lavenderblush@, @lawngreen@, @lemonchiffon@,
-- @lightblue@, @lightcoral@, @lightcyan@, @lightgoldenrodyellow@,
-- @lightgray@, @lightgreen@, @lightgrey@, @lightpink@, @lightsalmon@,
-- @lightseagreen@, @lightskyblue@, @lightslategray@, @lightslategrey@,
-- @lightsteelblue@, @lightyellow@, @lime@, @limegreen@, @linen@,
-- @magenta@, @maroon@, @mediumaquamarine@, @mediumblue@, @mediumorchid@,
-- @mediumpurple@, @mediumseagreen@, @mediumslateblue@, @mediumspringgreen@,
-- @mediumturquoise@, @mediumvioletred@, @midnightblue@, @mintcream@,
-- @mistyrose@, @moccasin@, @navajowhite@, @navy@, @oldlace@, @olive@,
-- @olivedrab@, @orange@, @orangered@, @orchid@, @palegoldenrod@,
-- @palegreen@, @paleturquoise@, @palevioletred@, @papayawhip@,
-- @peachpuff@, @peru@, @pink@, @plum@, @powderblue@, @purple@,
-- @rebeccapurple@, @red@, @rosybrown@, @royalblue@, @saddlebrown@, @salmon@,
-- @sandybrown@, @seagreen@, @seashell@, @sienna@, @silver@, @skyblue@,
-- @slateblue@, @slategray@, @slategrey@, @snow@, @springgreen@, @steelblue@,
-- @tan@, @teal@, @thistle@, @tomato@, @turquoise@, @violet@, @wheat@, @white@,
-- @whitesmoke@, @yellow@, and @yellowgreen@.
colorNamed :: Text -> Color
colorNamed (name) = colorNamed' (lowercase (substitution (name, " ", "")))

colorNamed' (name)
  | head == "#" && length (chars) == 7 = RGB (m r, m g, m b)
  | otherwise = byName (name)
  where
    chars = characters (name)
    head = chars # 1
    r = rest (chars, 1)
    g = rest (r, 2)
    b = rest (g, 2)
    m x = fromHex (first (x, 2))

fromHex [hi, lo] = (h (hi) * 16 + h (lo)) / 255
  where
    h ("0") = 0
    h ("1") = 1
    h ("2") = 2
    h ("3") = 3
    h ("4") = 4
    h ("5") = 5
    h ("6") = 6
    h ("7") = 7
    h ("8") = 8
    h ("9") = 9
    h ("a") = 10
    h ("b") = 11
    h ("c") = 12
    h ("d") = 13
    h ("e") = 14
    h ("f") = 15

byName ("aliceblue") = colorNamed' ("#f0f8ff")
byName ("antiquewhite") = colorNamed' ("#faebd7")
byName ("aqua") = colorNamed' ("#00ffff")
byName ("aquamarine") = colorNamed' ("#7fffd4")
byName ("azure") = colorNamed' ("#f0ffff")
byName ("beige") = colorNamed' ("#f5f5dc")
byName ("bisque") = colorNamed' ("#ffe4c4")
byName ("black") = colorNamed' ("#000000")
byName ("blanchedalmond") = colorNamed' ("#ffebcd")
byName ("blue") = colorNamed' ("#0000ff")
byName ("blueviolet") = colorNamed' ("#8a2be2")
byName ("brown") = colorNamed' ("#a52a2a")
byName ("burlywood") = colorNamed' ("#deb887")
byName ("cadetblue") = colorNamed' ("#5f9ea0")
byName ("chartreuse") = colorNamed' ("#7fff00")
byName ("chocolate") = colorNamed' ("#d2691e")
byName ("coral") = colorNamed' ("#ff7f50")
byName ("cornflowerblue") = colorNamed' ("#6495ed")
byName ("cornsilk") = colorNamed' ("#fff8dc")
byName ("crimson") = colorNamed' ("#dc143c")
byName ("cyan") = colorNamed' ("#00ffff")
byName ("darkblue") = colorNamed' ("#00008b")
byName ("darkcyan") = colorNamed' ("#008b8b")
byName ("darkgoldenrod") = colorNamed' ("#b8860b")
byName ("darkgray") = colorNamed' ("#a9a9a9")
byName ("darkgreen") = colorNamed' ("#006400")
byName ("darkgrey") = colorNamed' ("#a9a9a9")
byName ("darkkhaki") = colorNamed' ("#bdb76b")
byName ("darkmagenta") = colorNamed' ("#8b008b")
byName ("darkolivegreen") = colorNamed' ("#556b2f")
byName ("darkorange") = colorNamed' ("#ff8c00")
byName ("darkorchid") = colorNamed' ("#9932cc")
byName ("darkred") = colorNamed' ("#8b0000")
byName ("darksalmon") = colorNamed' ("#e9967a")
byName ("darkseagreen") = colorNamed' ("#8fbc8f")
byName ("darkslateblue") = colorNamed' ("#483d8b")
byName ("darkslategray") = colorNamed' ("#2f4f4f")
byName ("darkslategrey") = colorNamed' ("#2f4f4f")
byName ("darkturquoise") = colorNamed' ("#00ced1")
byName ("darkviolet") = colorNamed' ("#9400d3")
byName ("deeppink") = colorNamed' ("#ff1493")
byName ("deepskyblue") = colorNamed' ("#00bfff")
byName ("dimgray") = colorNamed' ("#696969")
byName ("dimgrey") = colorNamed' ("#696969")
byName ("dodgerblue") = colorNamed' ("#1e90ff")
byName ("firebrick") = colorNamed' ("#b22222")
byName ("floralwhite") = colorNamed' ("#fffaf0")
byName ("forestgreen") = colorNamed' ("#228b22")
byName ("fuchsia") = colorNamed' ("#ff00ff")
byName ("gainsboro") = colorNamed' ("#dcdcdc")
byName ("ghostwhite") = colorNamed' ("#f8f8ff")
byName ("gold") = colorNamed' ("#ffd700")
byName ("goldenrod") = colorNamed' ("#daa520")
byName ("gray") = colorNamed' ("#808080")
byName ("green") = colorNamed' ("#008000")
byName ("greenyellow") = colorNamed' ("#adff2f")
byName ("grey") = colorNamed' ("#808080")
byName ("honeydew") = colorNamed' ("#f0fff0")
byName ("hotpink") = colorNamed' ("#ff69b4")
byName ("indianred") = colorNamed' ("#cd5c5c")
byName ("indigo") = colorNamed' ("#4b0082")
byName ("ivory") = colorNamed' ("#fffff0")
byName ("khaki") = colorNamed' ("#f0e68c")
byName ("lavender") = colorNamed' ("#e6e6fa")
byName ("lavenderblush") = colorNamed' ("#fff0f5")
byName ("lawngreen") = colorNamed' ("#7cfc00")
byName ("lemonchiffon") = colorNamed' ("#fffacd")
byName ("lightblue") = colorNamed' ("#add8e6")
byName ("lightcoral") = colorNamed' ("#f08080")
byName ("lightcyan") = colorNamed' ("#e0ffff")
byName ("lightgoldenrodyellow") = colorNamed' ("#fafad2")
byName ("lightgray") = colorNamed' ("#d3d3d3")
byName ("lightgreen") = colorNamed' ("#90ee90")
byName ("lightgrey") = colorNamed' ("#d3d3d3")
byName ("lightpink") = colorNamed' ("#ffb6c1")
byName ("lightsalmon") = colorNamed' ("#ffa07a")
byName ("lightseagreen") = colorNamed' ("#20b2aa")
byName ("lightskyblue") = colorNamed' ("#87cefa")
byName ("lightslategray") = colorNamed' ("#778899")
byName ("lightslategrey") = colorNamed' ("#778899")
byName ("lightsteelblue") = colorNamed' ("#b0c4de")
byName ("lightyellow") = colorNamed' ("#ffffe0")
byName ("lime") = colorNamed' ("#00ff00")
byName ("limegreen") = colorNamed' ("#32cd32")
byName ("linen") = colorNamed' ("#faf0e6")
byName ("magenta") = colorNamed' ("#ff00ff")
byName ("maroon") = colorNamed' ("#800000")
byName ("mediumaquamarine") = colorNamed' ("#66cdaa")
byName ("mediumblue") = colorNamed' ("#0000cd")
byName ("mediumorchid") = colorNamed' ("#ba55d3")
byName ("mediumpurple") = colorNamed' ("#9370db")
byName ("mediumseagreen") = colorNamed' ("#3cb371")
byName ("mediumslateblue") = colorNamed' ("#7b68ee")
byName ("mediumspringgreen") = colorNamed' ("#00fa9a")
byName ("mediumturquoise") = colorNamed' ("#48d1cc")
byName ("mediumvioletred") = colorNamed' ("#c71585")
byName ("midnightblue") = colorNamed' ("#191970")
byName ("mintcream") = colorNamed' ("#f5fffa")
byName ("mistyrose") = colorNamed' ("#ffe4e1")
byName ("moccasin") = colorNamed' ("#ffe4b5")
byName ("navajowhite") = colorNamed' ("#ffdead")
byName ("navy") = colorNamed' ("#000080")
byName ("oldlace") = colorNamed' ("#fdf5e6")
byName ("olive") = colorNamed' ("#808000")
byName ("olivedrab") = colorNamed' ("#6b8e23")
byName ("orange") = colorNamed' ("#ffa500")
byName ("orangered") = colorNamed' ("#ff4500")
byName ("orchid") = colorNamed' ("#da70d6")
byName ("palegoldenrod") = colorNamed' ("#eee8aa")
byName ("palegreen") = colorNamed' ("#98fb98")
byName ("paleturquoise") = colorNamed' ("#afeeee")
byName ("palevioletred") = colorNamed' ("#db7093")
byName ("papayawhip") = colorNamed' ("#ffefd5")
byName ("peachpuff") = colorNamed' ("#ffdab9")
byName ("peru") = colorNamed' ("#cd853f")
byName ("pink") = colorNamed' ("#ffc0cb")
byName ("plum") = colorNamed' ("#dda0dd")
byName ("powderblue") = colorNamed' ("#b0e0e6")
byName ("purple") = colorNamed' ("#800080")
byName ("rebeccapurple") = colorNamed' ("#663399")
byName ("red") = colorNamed' ("#ff0000")
byName ("rosybrown") = colorNamed' ("#bc8f8f")
byName ("royalblue") = colorNamed' ("#4169e1")
byName ("saddlebrown") = colorNamed' ("#8b4513")
byName ("salmon") = colorNamed' ("#fa8072")
byName ("sandybrown") = colorNamed' ("#f4a460")
byName ("seagreen") = colorNamed' ("#2e8b57")
byName ("seashell") = colorNamed' ("#fff5ee")
byName ("sienna") = colorNamed' ("#a0522d")
byName ("silver") = colorNamed' ("#c0c0c0")
byName ("skyblue") = colorNamed' ("#87ceeb")
byName ("slateblue") = colorNamed' ("#6a5acd")
byName ("slategray") = colorNamed' ("#708090")
byName ("slategrey") = colorNamed' ("#708090")
byName ("snow") = colorNamed' ("#fffafa")
byName ("springgreen") = colorNamed' ("#00ff7f")
byName ("steelblue") = colorNamed' ("#4682b4")
byName ("tan") = colorNamed' ("#d2b48c")
byName ("teal") = colorNamed' ("#008080")
byName ("thistle") = colorNamed' ("#d8bfd8")
byName ("tomato") = colorNamed' ("#ff6347")
byName ("turquoise") = colorNamed' ("#40e0d0")
byName ("violet") = colorNamed' ("#ee82ee")
byName ("wheat") = colorNamed' ("#f5deb3")
byName ("white") = colorNamed' ("#ffffff")
byName ("whitesmoke") = colorNamed' ("#f5f5f5")
byName ("yellow") = colorNamed' ("#ffff00")
byName ("yellowgreen") = colorNamed' ("#9acd32")
byName (name) = error msg
  where
    msg = joined ["Unknown color named \"", name, "\""]
