{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

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
module CodeWorld.Reflex
    {-# WARNING "This is an experimental API.  It can change at any time." #-}
    (
    -- * Entry Point
      reflexOf
    , ReactiveInput
    , keyPress
    , keyRelease
    , textEntry
    , pointerPress
    , pointerRelease
    , pointerPosition
    , pointerDown
    , currentTime
    , timePassing
    -- * Pictures
    , Picture
    , TextStyle(..)
    , Font(..)
    , blank
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
    , rectangle
    , solidRectangle
    , thickRectangle
    , circle
    , solidCircle
    , thickCircle
    , arc
    , sector
    , thickArc
    , lettering
    , styledLettering
    , colored
    , coloured
    , translated
    , scaled
    , dilated
    , rotated
    , pictures
    , (<>)
    , (&)
    , coordinatePlane
    , codeWorldLogo
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
    , yellow
    , orange
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
    ) where

import CodeWorld.Driver
import CodeWorld.Picture
import CodeWorld.Color
import Control.Monad.Fix
import Reflex

reflexOf
    :: (forall t m. (Reflex t, MonadHold t m, MonadFix m)
        => ReactiveInput t -> m (Dynamic t Picture))
    -> IO ()
reflexOf = runReactive
