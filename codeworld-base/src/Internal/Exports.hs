{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

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

module Internal.Exports (
    -- Miscellaneous types and functions
    Number,
    Text,

    -- * Colors
    Color(..),
    Colour,
    black,
    white,
    red,
    green,
    blue,
    cyan,
    magenta,
    yellow,
    aquamarine,
    orange,
    azure,
    violet,
    chartreuse,
    rose,
    brown,
    pink,
    purple,
    gray,
    grey,
    mixed,
    lighter,
    light,
    darker,
    dark,
    brighter,
    bright,
    duller,
    dull,
    translucent,
    hue,
    saturation,
    luminosity,
    fromHSL,

    -- * Pictures
    Point,
    Vector,
    vectorSum,
    vectorDifference,
    scaledVector,
    rotatedVector,
    dotProduct,
    Picture,
    Font(..),
    TextStyle(..),
    blank,
    path,
    thickPath,
    polygon,
    thickPolygon,
    solidPolygon,
    curve,
    thickCurve,
    loop,
    thickLoop,
    solidLoop,
    rectangle,
    solidRectangle,
    thickRectangle,
    circle,
    solidCircle,
    thickCircle,
    arc,
    sector,
    thickArc,
    text,
    styledText,
    colored,
    coloured,
    translated,
    scaled,
    dilated,
    rotated,
    pictures,
    (&),
    coordinatePlane,
    codeWorldLogo,

    -- * Events
    Event(..),
    MouseButton(..),

    -- * Debugging
    traced,

    -- * Entry points
    Program,
    drawingOf,
    animationOf,
    simulationOf,
    interactionOf,
    gameOf
    ) where

import "base" Prelude (IO)

import Internal.Num
import Internal.Prelude
import Internal.Text

import Internal.Color
import Internal.Picture
import Internal.Event
import Internal.CodeWorld
