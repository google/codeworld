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

-- | Module for using CodeWorld pictures in Reflex-based FRP applications.
module CodeWorld.Reflex (
    -- $intro
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

-- $intro
-- = Using Reflex with CodeWorld
--
-- This is an alternative to the standard CodeWorld API, which is based on
-- the Reflex library.  You should import this *instead* of 'CodeWorld', since
-- the 'CodeWorld' module exports conflict with Reflex names.
--
-- You'll provide a function whose input can be used to access the user's
-- actions with keys, the mouse pointer, and time, and whose output is a
-- 'Picture'.  The 'Picture' value is build with the same combinators as the
-- main 'CodeWorld' library.
--
-- A simple example:
--
-- @
--     import CodeWorld.Reflex
--     import Reflex
--
--     main :: IO ()
--     main = reflexOf $ \\input -> do
--         angle <- foldDyn (+) 0 (gate (current (pointerDown input)) (timePassing input))
--         return $ (uncurry translated \<$> pointerPosition input \<*>)
--                $ (colored \<$> bool red green \<$> pointerDown input \<*>)
--                $ (rotated \<$> angle \<*>)
--                $ constDyn (solidRectangle 2 2)
-- @

-- | The entry point for running Reflex-based CodeWorld programs.
reflexOf
    :: (forall t m. (Reflex t, MonadHold t m, MonadFix m)
        => ReactiveInput t -> m (Dynamic t Picture))
    -> IO ()
reflexOf = runReactive
