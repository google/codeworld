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

module CodeWorld (
    -- Miscellaneous types and functions
    Number,
    Text,
    shuffle,

    -- * Colors
    Color(..),
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
    light,
    dark,
    bright,
    muted,
    translucent,

    -- * Pictures
    Point,
    Vector,
    addVectors,
    scaleVector,
    rotateVector,
    Picture,
    blank,
    line,
    thickLine,
    polygon,
    thickPolygon,
    solidPolygon,
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
    color,
    translate,
    scale,
    rotate,
    pictures,
    (&),
    coordinatePlane,
    codeWorldLogo,

    -- * Events
    Event(..),

    -- * Entry points
    Program,
    pictureOf,
    animationOf,
    simulationOf,
    interactionOf
    ) where

import "base" Prelude (IO)

import Internal.Num
import Internal.Prelude
import Internal.Text

import Internal.Color
import Internal.Picture
import Internal.Event
import Internal.CodeWorld
