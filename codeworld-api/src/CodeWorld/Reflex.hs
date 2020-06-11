{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

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

-- | Module for using CodeWorld pictures in Reflex-based FRP applications.
module CodeWorld.Reflex
  ( -- $intro
    reflexOf,
    debugReflexOf,
    ReflexCodeWorld,
    getKeyPress,
    getKeyRelease,
    getTextEntry,
    getPointerClick,
    getPointerPosition,
    isPointerDown,
    getTimePassing,
    draw,

    -- * Pictures
    Picture,
    blank,
    polyline,
    thickPolyline,
    polygon,
    thickPolygon,
    solidPolygon,
    curve,
    thickCurve,
    closedCurve,
    thickClosedCurve,
    solidClosedCurve,
    rectangle,
    solidRectangle,
    thickRectangle,
    circle,
    solidCircle,
    thickCircle,
    arc,
    sector,
    thickArc,
    lettering,
    TextStyle (..),
    Font (..),
    styledLettering,
    colored,
    coloured,
    translated,
    scaled,
    dilated,
    rotated,
    reflected,
    clipped,
    pictures,
    (<>),
    (&),
    coordinatePlane,
    codeWorldLogo,
    Point,
    translatedPoint,
    rotatedPoint,
    scaledPoint,
    dilatedPoint,
    Vector,
    vectorLength,
    vectorDirection,
    vectorSum,
    vectorDifference,
    scaledVector,
    rotatedVector,
    dotProduct,

    -- * Colors
    Color (..),
    Colour,
    pattern RGB,
    pattern HSL,
    black,
    white,
    red,
    green,
    blue,
    yellow,
    orange,
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
    assortedColors,
    hue,
    saturation,
    luminosity,
    alpha,
  )
where

import CodeWorld.Color
import CodeWorld.Driver
import CodeWorld.Picture
import Control.Monad.Fix
import Control.Monad.Trans
import Data.Bool
import qualified Data.Text as T
import Numeric (showFFloatAlt)
import Reflex

-- $intro
-- = Using Reflex with CodeWorld
--
-- This is an alternative to the standard CodeWorld API, which is based on
-- the Reflex library.  You should import this __instead__ of 'CodeWorld', since
-- the 'CodeWorld' module exports conflict with Reflex names.
--
-- When using this module, you can build pictures using the same combinators as
-- the main 'CodeWorld' module.  However, the way you handle user input and draw
-- to the screen is different.  The 'ReflexCodeWorld' constraint gives you a
-- a monad that has access to Reflex versions of input, such as keys, the mouse
-- pointer, and the time.  Based on these inputs, you'll use 'draw' to draw
-- output to the screen.
--
-- The Reflex API is documented in many places, but a great reference is
-- available in the <https://github.com/reflex-frp/reflex/blob/develop/Quickref.md Reflex Quick Reference>.

-- | Runs a reactive program, discharging the 'ReflexCodeWorld' constraint.
-- This is the starting point for Reflex programs in CodeWorld.
reflexOf :: (forall t m. ReflexCodeWorld t m => m ()) -> IO ()
reflexOf program = runReactive $ \input -> runReactiveProgram program input

-- | A variant of 'reflexOf' that includes some on-screen debugging controls.
-- You can use this during development, but should usually switch back to
-- 'reflexOf' when you're done debugging.
debugReflexOf :: (forall t m. ReflexCodeWorld t m => m ()) -> IO ()
debugReflexOf program = runReactive $ \input -> flip runReactiveProgram input $ do
  hoverAlpha <- getHoverAlpha
  controlState <- reactiveDebugControls hoverAlpha
  logicalInputs <- makeLogicalInputs controlState =<< getReactiveInput
  withReactiveInput logicalInputs program

data ControlState t = ControlState
  { csRunning :: Dynamic t Bool,
    csTimeDilation :: Dynamic t Double,
    csPointTransform :: Dynamic t (Point -> Point),
    csSyntheticStep :: Event t ()
  }

makeLogicalInputs :: (Reflex t, MonadHold t m) => ControlState t -> ReactiveInput t -> m (ReactiveInput t)
makeLogicalInputs (ControlState {..}) input = do
  keyPress <- return $ gateDyn csRunning $ keyPress input
  keyRelease <- return $ gateDyn csRunning $ keyRelease input
  textEntry <- return $ gateDyn csRunning $ textEntry input
  pointerPress <- return $ gateDyn csRunning $ attachWith ($) (current csPointTransform) (pointerPress input)
  pointerRelease <- return $ gateDyn csRunning $ attachWith ($) (current csPointTransform) (pointerRelease input)
  pointerPosition <- freezeDyn csRunning $ csPointTransform <*> pointerPosition input
  pointerDown <- freezeDyn csRunning $ pointerDown input
  timePassing <-
    return $
      mergeWith
        (+)
        [ gateDyn csRunning $ attachWith (*) (current csTimeDilation) (timePassing input),
          0.1 <$ csSyntheticStep
        ]
  return (ReactiveInput {..})

freezeDyn :: (Reflex t, MonadHold t m) => Dynamic t Bool -> Dynamic t a -> m (Dynamic t a)
freezeDyn predicate dyn = do
  initial <- sample (current dyn)
  holdDyn initial (gateDyn predicate (updated dyn))

reactiveDebugControls ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Double ->
  ReactiveProgram t m (ControlState t)
reactiveDebugControls hoverAlpha = do
  fastForwardClick <- fastForwardButton hoverAlpha (-4, -9)
  rec speedDragged <- speedSlider hoverAlpha (-6, -9) speedFactor
      playPauseClick <- playPauseButton hoverAlpha running (-8, -9)
      speedFactor <-
        foldDyn ($) 1 $
          mergeWith
            (.)
            [ (\s -> if s == 0 then 1 else 0) <$ playPauseClick,
              (\s -> max 2.0 (s + 1)) <$ fastForwardClick,
              const <$> speedDragged
            ]
      let running = (> 0) <$> speedFactor
  rec resetViewClick <- resetViewButton hoverAlpha (9, -3) needsReset
      zoomFactor <- zoomControls hoverAlpha (9, -6) resetViewClick
      panOffset <- panControls running resetViewClick
      let needsReset =
            (||) <$> ((/= 1) <$> zoomFactor)
              <*> ((/= (0, 0)) <$> panOffset)
  stepClick <- stepButton hoverAlpha (-2, -9) running
  transformUserPicture $ uncurry translated <$> panOffset
  transformUserPicture $ dilated <$> zoomFactor
  return $
    ControlState
      { csRunning = running,
        csTimeDilation = speedFactor,
        csPointTransform = transformPoint <$> zoomFactor <*> panOffset,
        csSyntheticStep = stepClick
      }
  where
    transformPoint z (dx, dy) (x, y) = ((x - dx) / z, (y - dy) / z)

getHoverAlpha :: ReflexCodeWorld t m => m (Dynamic t Double)
getHoverAlpha = do
  time <- getTimePassing
  move <- updated <$> getPointerPosition
  rec timeSinceMove <-
        foldDyn ($) 999 $
          mergeWith
            (.)
            [ (+) <$> gateDyn ((< 5) <$> timeSinceMove) time,
              const 0 <$ move
            ]
  return (alphaFromTime <$> timeSinceMove)
  where
    alphaFromTime t
      | t < 4.5 = 1
      | t > 5.0 = 0
      | otherwise = 10 - 2 * t

playPauseButton ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Double ->
  Dynamic t Bool ->
  Point ->
  ReactiveProgram t m (Event t ())
playPauseButton hoverAlpha running pos = do
  systemDraw $
    uncurry translated pos
      <$> (bool (playButton <$> hoverAlpha) (pauseButton <$> hoverAlpha) =<< running)
  click <- ffilter (onRect 0.8 0.8 pos) <$> getPointerClick
  return $ () <$ click
  where
    playButton a =
      colored
        (RGBA 0 0 0 a)
        (solidPolygon [(-0.2, 0.25), (-0.2, -0.25), (0.2, 0)])
        <> colored (RGBA 0.2 0.2 0.2 a) (rectangle 0.8 0.8)
        <> colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 0.8 0.8)
    pauseButton a =
      colored
        (RGBA 0 0 0 a)
        ( translated (-0.15) 0 (solidRectangle 0.2 0.6)
            <> translated 0.15 0 (solidRectangle 0.2 0.6)
        )
        <> colored (RGBA 0.2 0.2 0.2 a) (rectangle 0.8 0.8)
        <> colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 0.8 0.8)

stepButton ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Double ->
  Point ->
  Dynamic t Bool ->
  ReactiveProgram t m (Event t ())
stepButton hoverAlpha pos running = do
  systemDraw $
    uncurry translated pos
      <$> (bool (button <$> hoverAlpha) (constDyn blank) =<< running)
  click <- gateDyn (not <$> running) <$> ffilter (onRect 0.8 0.8 pos) <$> getPointerClick
  return $ () <$ click
  where
    button a =
      colored
        (RGBA 0 0 0 a)
        ( translated (-0.15) 0 (solidRectangle 0.2 0.5)
            <> solidPolygon [(0.05, 0.25), (0.05, -0.25), (0.3, 0)]
        )
        <> colored (RGBA 0.2 0.2 0.2 a) (rectangle 0.8 0.8)
        <> colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 0.8 0.8)

fastForwardButton ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Double ->
  Point ->
  ReactiveProgram t m (Event t ())
fastForwardButton hoverAlpha pos = do
  systemDraw $ uncurry translated pos <$> button <$> hoverAlpha
  click <- ffilter (onRect 0.8 0.8 pos) <$> getPointerClick
  return $ () <$ click
  where
    button a =
      colored
        (RGBA 0 0 0 a)
        ( solidPolygon [(-0.3, 0.25), (-0.3, -0.25), (-0.05, 0)]
            <> solidPolygon [(0.05, 0.25), (0.05, -0.25), (0.3, 0)]
        )
        <> colored (RGBA 0.2 0.2 0.2 a) (rectangle 0.8 0.8)
        <> colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 0.8 0.8)

speedSlider ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Double ->
  Point ->
  Dynamic t Double ->
  ReactiveProgram t m (Event t Double)
speedSlider hoverAlpha pos speedFactor = do
  systemDraw $ uncurry translated pos <$> (slider <$> hoverAlpha <*> speedFactor)
  click <- ffilter (onRect 3.0 0.8 pos) <$> getPointerClick
  release <- ffilter not <$> updated <$> isPointerDown
  dragging <- holdDyn False $ mergeWith (&&) [True <$ click, False <$ release]
  pointer <- getPointerPosition
  return $ speedFromPoint <$> mergeWith const [gateDyn dragging (updated pointer), click]
  where
    speedFromPoint (x, _y) = scaleRange (-1.4, 1.4) (0, 5) (x - fst pos)
    xFromSpeed speed = scaleRange (0, 5) (-1.4, 1.4) speed
    slider a speed =
      let xoff = xFromSpeed speed
       in colored
            (RGBA 0 0 0 a)
            ( translated xoff 0.75 $ scaled 0.5 0.5 $
                lettering (T.pack (showFFloatAlt (Just 2) speed "x"))
            )
            <> colored (RGBA 0 0 0 a) (translated xoff 0 (solidRectangle 0.2 0.8))
            <> colored (RGBA 0.2 0.2 0.2 a) (rectangle 2.8 0.25)
            <> colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 2.8 0.25)

resetViewButton ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Double ->
  Point ->
  Dynamic t Bool ->
  ReactiveProgram t m (Event t ())
resetViewButton hoverAlpha pos needsReset = do
  click <- gateDyn needsReset . ffilter (onRect 0.8 0.8 pos) <$> getPointerClick
  systemDraw $ uncurry translated pos <$> (bool (constDyn blank) (button <$> hoverAlpha) =<< needsReset)
  return $ () <$ click
  where
    button a =
      colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 0.7 0.2)
        <> colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 0.2 0.7)
        <> colored (RGBA 0.0 0.0 0.0 a) (thickRectangle 0.1 0.5 0.5)
        <> colored (RGBA 0.2 0.2 0.2 a) (rectangle 0.8 0.8)
        <> colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 0.8 0.8)

panControls ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Bool ->
  Event t () ->
  ReactiveProgram t m (Dynamic t (Double, Double))
panControls running resetClick = do
  click <- gateDyn (not <$> running) <$> getPointerClick
  release <- ffilter not <$> updated <$> isPointerDown
  dragging <- holdDyn False $ mergeWith (&&) [True <$ click, False <$ release]
  pos <- getPointerPosition
  let dragPos = bool (const Nothing) Just <$> dragging <*> pos
  diffPairs <- foldDyn (\x (y, _) -> (x, y)) (Nothing, Nothing) (updated dragPos)
  let drags = fmapMaybe toMovement (updated diffPairs)
  foldDyn ($) (0, 0) $
    mergeWith
      (.)
      [ vectorSum <$> drags,
        const (0, 0) <$ resetClick
      ]
  where
    toMovement (Just (x1, y1), Just (x2, y2)) = Just (x1 - x2, y1 - y2)
    toMovement _ = Nothing

zoomControls ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Double ->
  Point ->
  Event t () ->
  ReactiveProgram t m (Dynamic t Double)
zoomControls hoverAlpha (x, y) resetClick = do
  zoomInClick <- zoomInButton hoverAlpha (x, y + 2)
  zoomOutClick <- zoomOutButton hoverAlpha (x, y - 2)
  rec zoomDrag <- zoomSlider hoverAlpha (x, y) zoomFactor
      zoomFactor <-
        foldDyn ($) 1 $
          mergeWith
            (.)
            [ (* zoomIncrement) <$ zoomInClick,
              (/ zoomIncrement) <$ zoomOutClick,
              const <$> zoomDrag,
              const 1 <$ resetClick
            ]
  return zoomFactor

zoomInButton ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Double ->
  Point ->
  ReactiveProgram t m (Event t ())
zoomInButton hoverAlpha pos = do
  systemDraw $ uncurry translated pos <$> button <$> hoverAlpha
  (() <$) <$> ffilter (onRect 0.8 0.8 pos) <$> getPointerClick
  where
    button a =
      colored
        (RGBA 0 0 0 a)
        ( translated
            (-0.05)
            (0.05)
            ( thickCircle 0.1 0.22
                <> solidRectangle 0.06 0.25
                <> solidRectangle 0.25 0.06
                <> rotated (- pi / 4) (translated 0.35 0 (solidRectangle 0.2 0.1))
            )
        )
        <> colored (RGBA 0.2 0.2 0.2 a) (rectangle 0.8 0.8)
        <> colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 0.8 0.8)

zoomOutButton ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Double ->
  Point ->
  ReactiveProgram t m (Event t ())
zoomOutButton hoverAlpha pos = do
  systemDraw $ uncurry translated pos <$> button <$> hoverAlpha
  (() <$) <$> ffilter (onRect 0.8 0.8 pos) <$> getPointerClick
  where
    button a =
      colored
        (RGBA 0 0 0 a)
        ( translated
            (-0.05)
            (0.05)
            ( thickCircle 0.1 0.22
                <> solidRectangle 0.25 0.06
                <> rotated (- pi / 4) (translated 0.35 0 (solidRectangle 0.2 0.1))
            )
        )
        <> colored (RGBA 0.2 0.2 0.2 a) (rectangle 0.8 0.8)
        <> colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 0.8 0.8)

zoomSlider ::
  ( PerformEvent t m,
    TriggerEvent t m,
    Adjustable t m,
    NotReady t m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Double ->
  Point ->
  Dynamic t Double ->
  ReactiveProgram t m (Event t Double)
zoomSlider hoverAlpha pos factor = do
  systemDraw $ uncurry translated pos <$> (slider <$> hoverAlpha <*> factor)
  click <- ffilter (onRect 0.8 3.0 pos) <$> getPointerClick
  release <- ffilter not <$> updated <$> isPointerDown
  dragging <- holdDyn False $ mergeWith (&&) [True <$ click, False <$ release]
  pointer <- getPointerPosition
  return $ zoomFromPoint <$> mergeWith const [gateDyn dragging (updated pointer), click]
  where
    zoomFromPoint (_x, y) = zoomIncrement ** (scaleRange (-1.4, 1.4) (-10, 10) (y - snd pos))
    yFromZoom z = scaleRange (-10, 10) (-1.4, 1.4) (logBase zoomIncrement z)
    slider a z =
      let yoff = yFromZoom z
       in colored
            (RGBA 0 0 0 a)
            ( translated (-1.1) yoff $ scaled 0.5 0.5 $
                lettering (T.pack (show (round (z * 100) :: Int) ++ "%"))
            )
            <> colored (RGBA 0 0 0 a) (translated 0 yoff (solidRectangle 0.8 0.2))
            <> colored (RGBA 0.2 0.2 0.2 a) (rectangle 0.25 2.8)
            <> colored (RGBA 0.8 0.8 0.8 a) (solidRectangle 0.25 2.8)

zoomIncrement :: Double
zoomIncrement = 8 ** (1 / 10)

onRect :: Double -> Double -> Point -> Point -> Bool
onRect w h (x1, y1) (x2, y2) = abs (x1 - x2) < w / 2 && abs (y1 - y2) < h / 2

scaleRange :: (Double, Double) -> (Double, Double) -> Double -> Double
scaleRange (a1, b1) (a2, b2) x = min b2 $ max a2 $ (x - a1) / (b1 - a1) * (b2 - a2) + a2
