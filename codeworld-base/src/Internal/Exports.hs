{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}

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
module Internal.Exports
    -- Miscellaneous types and functions
    ( Number
    , Text
    -- * Colors
    , Color(..)
    , Colour
    , pattern RGB
    , pattern HSL
    , black
    , white
    , red
    , green
    , blue
    , cyan
    , magenta
    , yellow
    , aquamarine
    , orange
    , azure
    , violet
    , chartreuse
    , rose
    , brown
    , pink
    , purple
    , gray
    , grey
    , mixed
    , lighter
    , light
    , darker
    , dark
    , brighter
    , bright
    , duller
    , dull
    , translucent
    , assortedColors
    , hue
    , saturation
    , luminosity
    , alpha
    -- * Pictures
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
    , Picture
    , Font(..)
    , TextStyle(..)
    , blank
    , polyline
    , path
    , thickPolyline
    , thickPath
    , polygon
    , thickPolygon
    , solidPolygon
    , curve
    , thickCurve
    , closedCurve
    , thickClosedCurve
    , solidClosedCurve
    , rectangle
    , solidRectangle
    , thickRectangle
    , circle
    , solidCircle
    , thickCircle
    , arc
    , sector
    , thickArc
    , text
    , lettering
    , styledText
    , styledLettering
    , colored
    , coloured
    , translated
    , scaled
    , dilated
    , rotated
    , pictures
    , (&)
    , coordinatePlane
    , codeWorldLogo
    -- * Events
    , Event(..)
    , MouseButton(..)
    , pattern PointerPress
    , pattern PointerRelease
    , pattern PointerMovement
    -- * Debugging
    , traced
    -- * Entry points
    , Program
    , drawingOf
    , animationOf
    , activityOf
    , debugActivityOf
    , groupActivityOf
    , simulationOf
    , debugSimulationOf
    , interactionOf
    , debugInteractionOf
    , collaborationOf
    ) where

import "base" Prelude (IO)

import Internal.Num
import Internal.Prelude
import Internal.Text

import Internal.CodeWorld
import Internal.Color
import Internal.Event
import Internal.Picture
