{-# LANGUAGE FlexibleContexts #-}
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
    -- * Old Entry Point
      reflexOf
    , debugReflexOf
    , ReactiveInput
    , keyPress
    , keyRelease
    , textEntry
    , pointerPress
    , pointerRelease
    , pointerPosition
    , pointerDown
    , timePassing
    -- * New Entry Point
    , reactiveOf
    , ReflexCodeWorld(..)
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
import Control.Monad.Trans
import qualified Data.Text as T
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
    :: (forall t m. (Reflex t, MonadHold t m, MonadFix m, PerformEvent t m,
                     MonadIO (Performable m), Adjustable t m, PostBuild t m)
        => ReactiveInput t -> m (Dynamic t Picture))
    -> IO ()
reflexOf program = runReactive $ \input -> do
    pic <- program input
    return (pic, pic)

debugReflexOf
    :: (forall t m. (Reflex t, MonadHold t m, MonadFix m)
        => ReactiveInput t -> m (Dynamic t Picture))
    -> IO ()
debugReflexOf program = runReactive
    $ withZoomAndPan (9, -4) (9, -8) (9, -6) (9, -3)
    $ \input -> do
        pic <- program input
        return (pic, pic)

splitEvent :: Reflex t => (a -> Bool) -> Event t a -> (Event t a, Event t a)
splitEvent predicate event =
    (ffilter predicate event, ffilter (not . predicate) event)

onRect :: Double -> Double -> Point -> Point -> Bool
onRect w h (x1, y1) (x2, y2) = abs (x1 - x2) < w / 2 && abs (y1 - y2) < h / 2

scaleRange :: (Double, Double) -> (Double, Double) -> Double -> Double
scaleRange (a1, b1) (a2, b2) x = min b2 $ max a2 $ (x - a1) / (b1 - a1) * (b2 - a2) + a2

withZoomAndPan
    :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
    => Point
    -> Point
    -> Point
    -> Point
    -> (ReactiveInput t -> m (Dynamic t Picture, Dynamic t Picture))
    -> (ReactiveInput t -> m (Dynamic t Picture, Dynamic t Picture))
withZoomAndPan zoomInPos zoomOutPos zoomSlidePos resetPos base input = do
    let (zoomInClick, a) = splitEvent (onRect 0.8 0.8 zoomInPos) (pointerPress input)
        (zoomOutClick, b) = splitEvent (onRect 0.8 0.8 zoomOutPos) a
        (zoomSlideClick, c) = splitEvent (onRect 0.8 3.0 zoomSlidePos) b
        (resetClick, baseClick) = splitEvent (onRect 0.8 0.8 resetPos) c
    draggingZoom <- holdDyn False $ mergeWith (&&) [
        True <$ zoomSlideClick,
        False <$ pointerRelease input
        ]
    let zoomDragged = gateDyn draggingZoom (updated (pointerPosition input))
    let zoomSet = mergeWith const [zoomDragged, zoomSlideClick]
    zoomFactor <- foldDyn ($) 1 $ mergeWith (.) [
        (* zoomIncrement) <$ zoomInClick,
        (/ zoomIncrement) <$ zoomOutClick,
        const . zoomFromPoint <$> zoomSet,
        const 1 <$ resetClick
        ]

    (inspectPic, displayPic) <- base $ input {
        pointerPress = attachPromptlyDynWith (\z (x, y) -> (x / z, y / z)) zoomFactor baseClick,
        pointerRelease = attachPromptlyDynWith (\z (x, y) -> (x / z, y / z)) zoomFactor (pointerRelease input)
        }

    return (inspectPic, pictures <$> sequence [
        constDyn (uncurry translated zoomInPos zoomInButton),
        constDyn (uncurry translated zoomOutPos zoomOutButton),
        constDyn (uncurry translated resetPos resetButton),
        uncurry translated zoomSlidePos <$> zoomSlider <$> zoomFactor,
        dilated <$> zoomFactor <*> displayPic
        ])
  where
    zoomIncrement = 8 ** (1/10)
    zoomFromPoint (_x, y) = zoomIncrement ** (scaleRange (-1.4, 1.4) (-10, 10) (y - snd (zoomSlidePos)))
    yFromZoom z = scaleRange (-10, 10) (-1.4, 1.4) (logBase zoomIncrement z)
    zoomInButton =
        colored
            (RGBA 0 0 0 1)
            (translated (-0.05) (0.05) (
                thickCircle 0.1 0.22 <>
                solidRectangle 0.06 0.25 <>
                solidRectangle 0.25 0.06 <>
                rotated (-pi / 4) (translated 0.35 0 (solidRectangle 0.2 0.1))
            )) <>
        colored (RGBA 0.2 0.2 0.2 1) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 1) (solidRectangle 0.8 0.8)
    zoomOutButton =
        colored
            (RGBA 0 0 0 1)
            (translated (-0.05) (0.05) (
                thickCircle 0.1 0.22 <>
                solidRectangle 0.25 0.06 <>
                rotated (-pi / 4) (translated 0.35 0 (solidRectangle 0.2 0.1))
            )) <>
        colored (RGBA 0.2 0.2 0.2 1) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 1) (solidRectangle 0.8 0.8)
    zoomSlider z = let yoff = yFromZoom z in
        colored
            (RGBA 0 0 0 1)
            (translated (-1.1) yoff $ scaled 0.5 0.5 $
                 lettering (T.pack (show (round (z * 100) :: Int) ++ "%"))) <>
        colored (RGBA 0 0 0 1) (translated 0 yoff (solidRectangle 0.8 0.2)) <>
        colored (RGBA 0.2 0.2 0.2 1) (rectangle 0.25 2.8) <>
        colored (RGBA 0.8 0.8 0.8 1) (solidRectangle 0.25 2.8)
    resetButton =
        colored (RGBA 0.8 0.8 0.8 1) (solidRectangle 0.7 0.2) <>
        colored (RGBA 0.8 0.8 0.8 1) (solidRectangle 0.2 0.7) <>
        colored (RGBA 0.0 0.0 0.0 1) (thickRectangle 0.1 0.5 0.5) <>
        colored (RGBA 0.2 0.2 0.2 1) (rectangle 0.8 0.8) <>
        colored (RGBA 0.8 0.8 0.8 1) (solidRectangle 0.8 0.8)

reactiveOf :: (forall t m. ReflexCodeWorld t m => m ()) -> IO ()
reactiveOf program = runReactive $ \input -> do
    (userPicture, displayPicture) <- runReactiveProgram program input
    return (userPicture, displayPicture)
