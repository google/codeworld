{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}
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

module CodeWorld.Parameter
  {-# WARNING "This is an experimental API.  It can change at any time." #-}
  ( Parameter,
    parametricDrawingOf,
    parameterOf,
    paramConversion,
    constant,
    toggle,
    slider,
    random,
    timer,
    currentHour,
    currentMinute,
    currentSecond,
  )
where

import CodeWorld
import CodeWorld.Driver (runInspect)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime
import Numeric (showFFloatAlt)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (newStdGen, randomR)

type Bounds = (Double, Double, Double, Double)

data Parameter where
  Parameter ::
    Text ->
    Double ->
    Picture ->
    Bounds ->
    (Event -> Parameter) ->
    Parameter

-- | A drawing that depends on parameters.  A parameter is a
parametricDrawingOf :: [Parameter] -> ([Double] -> Picture) -> IO ()
parametricDrawingOf initialParams mainPic =
  runInspect
    (zip [0 :: Int ..] (layout 0 (-9.5) 9.5 initialParams), True, 5)
    (const id)
    change
    picture
    rawPicture
  where
    layout _ _ _ [] = []
    layout maxw x y (p : ps)
      | y > (-9.5) + h + titleHeight =
        framedParam (x - left) (y - top - titleHeight) p
          : layout (max maxw w) x (y - h - titleHeight - gap) ps
      | otherwise = layout 0 (x + maxw + gap) 9.5 (p : ps)
      where
        Parameter _ _ _ (left, top, w, h) _ = p
        gap = 0.5
    change (KeyPress " ") (params, vis, _) = (params, not vis, 2)
    change (PointerPress pt) (params, vis, t) =
      case (vis, pullMatch (hitTest pt . snd) params) of
        (True, (Just p, ps)) -> (fmap (changeParam (PointerPress pt)) p : ps, vis, t)
        _ -> (params, vis, t)
    change event (params, vis, t) =
      (map (fmap (changeParam event)) params, vis, changeTime event t)
    picture (params, vis, t) =
      showHideBanner t
        & (if vis then pictures (map (showParam . snd) params) else blank)
        & rawPicture (params, vis, t)
    rawPicture (params, _, _) =
      mainPic (map (getParam . snd) (sortBy (compare `on` fst) params))
    changeParam event (Parameter _ _ _ _ handle) = handle event
    showParam (Parameter _ _ pic _ _) = pic
    getParam (Parameter _ val _ _ _) = val
    changeTime (TimePassing dt) t = max 0 (t - dt)
    changeTime _ t = t
    showHideBanner 0 = blank
    showHideBanner t =
      translated 0 (-9)
        $ dilated 0.5
        $ colored
          (RGBA 0 0 0 t)
          (rectangle 18 2 & lettering "Press Space to show/hide parameters.")
          & colored (RGBA 0.75 0.75 0.75 (min 0.8 t)) (solidRectangle 18 2)

pullMatch :: (a -> Bool) -> [a] -> (Maybe a, [a])
pullMatch _ [] = (Nothing, [])
pullMatch p (a : as)
  | p a = (Just a, as)
  | otherwise = fmap (a :) (pullMatch p as)

hitTest :: Point -> Parameter -> Bool
hitTest (x, y) (Parameter _ _ _ (left, top, w, h) _) =
  x > left && x < left + w && y < top && y > top - h

parameterOf ::
  Text ->
  state ->
  (Event -> state -> state) ->
  (state -> Double) ->
  (state -> Picture) ->
  (state -> Bounds) ->
  Parameter
parameterOf name initial change value picture bounds =
  Parameter
    name
    (value initial)
    (picture initial)
    (bounds initial)
    (\e -> parameterOf name (change e initial) change value picture bounds)

paramConversion :: (Double -> Double) -> Parameter -> Parameter
paramConversion c (Parameter name val pic bounds handle) =
  Parameter name (c val) pic bounds (paramConversion c . handle)

framedParam :: Double -> Double -> Parameter -> Parameter
framedParam ix iy iparam =
  parameterOf
    (paramName iparam)
    (iparam, (ix, iy), True, Nothing)
    frameHandle
    frameValue
    framePicture
    frameBounds
  where
    frameHandle (PointerPress (px, py)) (param, (x, y), open, anchor)
      | onOpenButton = (param, (x, y), not open, anchor)
      | onTitleBar = (param, (x, y), open, Just (px, py))
      where
        Parameter _ _ _ (left, top, w, h) _ = param
        onTitleBar =
          abs (px - x - (left + w / 2)) < w / 2
            && abs (py - y - top - titleHeight / 2) < titleHeight / 2
        onOpenButton
          | w * h > 0 =
            abs (px - x - (left + w - titleHeight / 2)) < 0.2
              && abs (py - y - (top + titleHeight / 2)) < 0.2
          | otherwise = False
    frameHandle (PointerRelease _) (param, loc, open, Just _) =
      (param, loc, open, Nothing)
    frameHandle (PointerMovement (px, py)) (param, (x, y), open, Just (ax, ay)) =
      (param, (x + px - ax, y + py - ay), open, Just (px, py))
    frameHandle (TimePassing dt) (Parameter _ _ _ _ handle, loc, open, anchor) =
      (handle (TimePassing dt), loc, open, anchor)
    frameHandle event (Parameter _ _ _ _ handle, (x, y), True, anchor) =
      (handle (untranslate x y event), (x, y), True, anchor)
    frameHandle _ other = other
    frameValue (Parameter _ v _ _ _, _, _, _) = v
    framePicture (param@(Parameter _ _ _ (left, top, w, h) _), (x, y), open, _) =
      translated x y $
        translated (left + w / 2) (top + titleHeight / 2) (titleBar param open)
          & translated (left + w / 2) (top - h / 2) (clientArea param open)
    frameBounds (Parameter _ _ _ (left, top, w, h) _, (x, y), True, _) =
      (x + left, y + top + titleHeight, w, h + titleHeight)
    frameBounds (Parameter _ _ _ (left, top, w, _) _, (x, y), False, _) =
      (x + left, y + top + titleHeight, w, titleHeight)
    titleBar (Parameter n v _ (_, _, w, h) _) open
      | w * h > 0 =
        rectangle w titleHeight
          & translated
            ((w - titleHeight) / 2)
            0
            (if open then collapseButton else expandButton)
          & translated
            (- titleHeight / 2)
            0
            ( clipped
                (w - titleHeight)
                titleHeight
                (dilated 0.5 (lettering (titleText n v)))
            )
          & colored titleColor (solidRectangle w titleHeight)
      | otherwise =
        rectangle w titleHeight
          & clipped w titleHeight (dilated 0.5 (lettering (titleText n v)))
          & colored titleColor (solidRectangle w titleHeight)
    titleText n v
      | T.length n > 10 = T.take 8 n <> "... = " <> formatVal v
      | otherwise = n <> " = " <> formatVal v
    collapseButton = rectangle 0.4 0.4 & solidPolygon [(-0.1, -0.1), (0.1, -0.1), (0, 0.1)]
    expandButton = rectangle 0.4 0.4 & solidPolygon [(-0.1, 0.1), (0.1, 0.1), (0, -0.1)]
    clientArea (Parameter _ _ pic (_, _, w, h) _) True
      | w * h > 0 =
        rectangle w h
          & clipped w h pic
          & colored bgColor (solidRectangle 5 1)
    clientArea _ _ = blank
    untranslate x y (PointerPress (px, py)) = PointerPress (px - x, py - y)
    untranslate x y (PointerRelease (px, py)) = PointerRelease (px - x, py - y)
    untranslate x y (PointerMovement (px, py)) = PointerMovement (px - x, py - y)
    untranslate _ _ other = other
    paramName (Parameter n _ _ _ _) = n
    formatVal v = pack (showFFloatAlt (Just 2) v "")

titleHeight :: Double
titleHeight = 0.7

constant :: Text -> Double -> Parameter
constant name n =
  parameterOf
    name
    n
    (const id)
    id
    (const blank)
    (const (-2.5, 0, 5, 0))

toggle :: Text -> Parameter
toggle name =
  parameterOf
    name
    False
    change
    value
    picture
    (const (-2.5, 0.5, 5, 1))
  where
    change (PointerPress (px, py))
      | abs px < 4, abs py < 1 = not
    change _ = id
    value True = 1
    value False = 0
    picture True = dilated 0.5 $ lettering "\x2611"
    picture False = dilated 0.5 $ lettering "\x2610"

slider :: Text -> Parameter
slider name =
  parameterOf
    name
    (0.5, False)
    change
    fst
    picture
    (const (-2.5, 0.5, 5, 1))
  where
    change (PointerPress (px, py)) (_, _)
      | abs px < 2, abs py < 0.25 = (min 1 $ max 0 $ (px + 2) / 4, True)
    change (PointerRelease _) (v, _) = (v, False)
    change (PointerMovement (px, _)) (_, True) =
      (min 1 $ max 0 $ (px + 2) / 4, True)
    change _ state = state
    picture (v, _) =
      translated (v * 4 - 2) 0 (solidRectangle 0.125 0.5)
        & solidRectangle 4 0.1

random :: Text -> Parameter
random name =
  parameterOf
    name
    (next (unsafePerformIO newStdGen))
    change
    fst
    (const $ dilated 0.5 $ lettering "\x21ba Regenerate")
    (const (-2.5, 0.5, 5, 1))
  where
    change (PointerPress (px, py))
      | abs px < 4, abs py < 1 = next . snd
    change _ = id
    next = randomR (0.0, 1.0)

timer :: Text -> Parameter
timer name =
  parameterOf
    name
    (0, 1)
    change
    fst
    picture
    (const (-2.5, 0.5, 5, 1))
  where
    change (TimePassing dt) (t, r) = (t + r * dt, r)
    change (PointerPress (px, py)) (t, r)
      | abs (px - 5 / 6) < 5 / 6, abs py < 0.75 = (t, 1 - r)
      | abs (px + 5 / 6) < 5 / 6, abs py < 0.75 = (0, 0)
    change _ state = state
    picture (_, 0) =
      (translated (5 / 6) 0 $ dilated 0.5 $ lettering "\x23e9")
        & (translated (-5 / 6) 0 $ dilated 0.5 $ lettering "\x23ee")
    picture _ =
      (translated (5 / 6) 0 $ dilated 0.5 $ lettering "\x23f8")
        & (translated (-5 / 6) 0 $ dilated 0.5 $ lettering "\x23ee")

currentHour :: Parameter
currentHour =
  parameterOf
    "hour"
    ()
    (const id)
    (\_ -> unsafePerformIO $ fromIntegral <$> todHour <$> getTimeOfDay)
    (const blank)
    (const (-2.5, 0, 5, 0))

currentMinute :: Parameter
currentMinute =
  parameterOf
    "minute"
    ()
    (const id)
    (\_ -> unsafePerformIO $ fromIntegral <$> todMin <$> getTimeOfDay)
    (const blank)
    (const (-2.5, 0, 5, 0))

currentSecond :: Parameter
currentSecond =
  parameterOf
    "second"
    ()
    (const id)
    (\_ -> unsafePerformIO $ realToFrac <$> todSec <$> getTimeOfDay)
    (const blank)
    (const (-2.5, 0, 5, 0))

getTimeOfDay :: IO TimeOfDay
getTimeOfDay = do
  now <- getCurrentTime
  timezone <- getCurrentTimeZone
  return (localTimeOfDay (utcToLocalTime timezone now))

titleColor :: Color
titleColor = RGBA 0.7 0.7 0.7 0.9

bgColor :: Color
bgColor = RGBA 0.8 0.85 0.95 0.8
