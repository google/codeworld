{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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
    -- * Entry points
      Program
    , drawingOf
    , animationOf
    , activityOf
    , debugActivityOf
    , groupActivityOf
    -- * Pictures
    , Picture
    , codeWorldLogo
    , circle
    , solidCircle
    , thickCircle
    , rectangle
    , solidRectangle
    , thickRectangle
    , pictures
    , (&)
    , coordinatePlane
    , blank
    , colored
    , coloured
    , translated
    , scaled
    , dilated
    , rotated
    , polyline
    , thickPolyline
    , polygon
    , thickPolygon
    , solidPolygon
    , curve
    , thickCurve
    , closedCurve
    , thickClosedCurve
    , solidClosedCurve
    , arc
    , sector
    , thickArc
    , lettering
    , styledLettering
    , Font(..)
    , TextStyle(..)
    -- * Colors
    , Color
    , Colour
    , pattern RGBA
    , pattern RGB
    , pattern HSL
    , black
    , white
    , red
    , green
    , blue
    , yellow
    , orange
    , brown
    , pink
    , purple
    , gray
    , grey
    , mixed
    , light
    , dark
    , bright
    , dull
    , translucent
    , assortedColors
    , lighter
    , darker
    , brighter
    , duller
    , hue
    , saturation
    , luminosity
    , alpha
    -- * Points and vectors
    , Point
    , translatedPoint
    , rotatedPoint
    , scaledPoint
    , dilatedPoint
    , Vector
    , vectorLength
    , vectorDirection
    , vectorSum
    , vectorDifference
    , scaledVector
    , rotatedVector
    , dotProduct
    -- * Events
    , Event(..)
    -- * Debugging
    , traced
    -- * Deprecated definitions (do not use)
    , path
    , thickPath
    , text
    , styledText
    , simulationOf
    , debugSimulationOf
    , interactionOf
    , debugInteractionOf
    , collaborationOf
    , cyan
    , magenta
    , aquamarine
    , azure
    , violet
    , chartreuse
    , rose
    ) where

import "base" Prelude (IO)

import Internal.Num
import Internal.Prelude
import Internal.Text

import Internal.CodeWorld
import Internal.Color
import Internal.Event
import Internal.Picture
