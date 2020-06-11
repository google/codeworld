{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}

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

module CodeWorld.Parameter
  {-# WARNING "This is an experimental API.  It can change at any time." #-}
  ( Parameter,
    parametricDrawingOf,
    slider,
    toggle,
    counter,
    constant,
    random,
    timer,
    currentHour,
    currentMinute,
    currentSecond,
    converted,
    renamed,
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

-- | Bounds information for a parameter UI.  The fields are the
-- left and top coordinate, then the width and height.
type Bounds = (Double, Double, Double, Double)

-- | The source for a parameter that can be adjusted in a parametric
-- drawing.  Parameters can get their values from sliders, buttons,
-- counters, timers, etc.
data Parameter where
  Parameter ::
    Text ->
    Double ->
    Picture ->
    Bounds ->
    (Event -> Parameter) ->
    Parameter

-- | A drawing that depends on parameters.  The first argument is a
-- list of parameters.  The second is a picture, which depends on the
-- values of those parameters.  Each number used to retrieve the picture
-- is the value of the corresponding parameter in the first list.
parametricDrawingOf :: [Parameter] -> ([Double] -> Picture) -> IO ()
parametricDrawingOf initialParams mainPic =
  runInspect
    (zip [0 :: Int ..] (layoutParams 0 (-9.5) 9.5 initialParams), True, 5)
    (const id)
    change
    picture
    rawPicture
  where
    change (KeyPress " ") (params, vis, _) = (params, not vis, 2)
    change (PointerPress pt) (params, vis, t) =
      case (vis, pullMatch (hitTest pt . snd) params) of
        (True, (Just p, ps)) -> (fmap (changeParam (PointerPress pt)) p : ps, vis, t)
        _ -> (params, vis, t)
    change event (params, vis, t) =
      (map (fmap (changeParam event)) params, vis, changeTime event t)
    picture (params, vis, t) =
      showHideBanner t
        & (picWhen vis $ pictures (map (showParam . snd) params))
        & rawPicture (params, vis, t)
    rawPicture (params, _, _) =
      mainPic (map (getParam . snd) (sortBy (compare `on` fst) params))
    changeParam event (Parameter _ _ _ _ handle) = handle event
    showParam (Parameter _ _ pic _ _) = pic
    getParam (Parameter _ val _ _ _) = val
    changeTime (TimePassing dt) t = max 0 (t - dt)
    changeTime _ t = t
    showHideBanner t =
      picWhen (t > 0)
        $ translated 0 (-9)
        $ dilated 0.5
        $ colored (RGBA 0 0 0 t) (rectangle 18 2)
          & colored
            (RGBA 0 0 0 t)
            (lettering "Press Space to show/hide parameters.")
          & colored (RGBA 0.75 0.75 0.75 (min 0.8 t)) (solidRectangle 18 2)

-- | Wraps a list of parameters in frames to lay them out on the screen.
layoutParams :: Double -> Double -> Double -> [Parameter] -> [Parameter]
layoutParams _ _ _ [] = []
layoutParams maxw x y (p : ps)
  | y > (-9.5) + h + titleHeight =
    framedParam (x - left) (y - top - titleHeight) p
      : layoutParams (max maxw w) x (y - h - titleHeight - gap) ps
  | otherwise = layoutParams 0 (x + maxw + gap) 9.5 (p : ps)
  where
    Parameter _ _ _ (left, top, w, h) _ = p
    gap = 0.5

-- | Finds the first element of a list that matches the predicate, if any,
-- and removes it from the list, returning it separately from the remaining
-- elements.
pullMatch :: (a -> Bool) -> [a] -> (Maybe a, [a])
pullMatch _ [] = (Nothing, [])
pullMatch p (a : as)
  | p a = (Just a, as)
  | otherwise = fmap (a :) (pullMatch p as)

-- | Determines if a point is inside the screen area for a given parameter.
hitTest :: Point -> Parameter -> Bool
hitTest (x, y) (Parameter _ _ _ (left, top, w, h) _) =
  x > left && x < left + w && y < top && y > top - h

-- | Builds a parameter from an explicit state.
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

-- Puts a simple parameter in a draggable widget that let the user
-- manipulate it on the screen, and displays the name and value.
-- All parameters are enclosed in one of these automatically.
framedParam :: Double -> Double -> Parameter -> Parameter
framedParam ix iy iparam =
  parameterOf
    name
    (iparam, (ix, iy), True, Nothing)
    framedHandle
    (\(Parameter _ v _ _ _, _, _, _) -> v)
    framedPicture
    framedBounds
  where
    (Parameter name _ _ _ _) = iparam

-- | The state of a framedParam, which includes the original parameter,
-- its location, whether it's open (expanded) or not, and the anchor if
-- it is currently being dragged.
type FrameState = (Parameter, Point, Bool, Maybe Point)

framedHandle :: Event -> FrameState -> FrameState
framedHandle (PointerPress (px, py)) (param, (x, y), open, anchor)
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
framedHandle (PointerRelease _) (param, loc, open, Just _) =
  (param, loc, open, Nothing)
framedHandle (PointerMovement (px, py)) (param, (x, y), open, Just (ax, ay)) =
  (param, (x + px - ax, y + py - ay), open, Just (px, py))
framedHandle (TimePassing dt) (Parameter _ _ _ _ handle, loc, open, anchor) =
  (handle (TimePassing dt), loc, open, anchor)
framedHandle event (Parameter _ _ _ _ handle, (x, y), True, anchor) =
  (handle (untranslated x y event), (x, y), True, anchor)
framedHandle _ other = other

framedPicture :: FrameState -> Picture
framedPicture (Parameter n v pic (left, top, w, h) _, (x, y), open, _) =
  translated x y $
    translated (left + w / 2) (top + titleHeight / 2) titleBar
      & translated (left + w / 2) (top - h / 2) clientArea
  where
    titleBar
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
                (dilated 0.5 (lettering titleText))
            )
          & colored titleColor (solidRectangle w titleHeight)
      | otherwise =
        rectangle w titleHeight
          & clipped w titleHeight (dilated 0.5 (lettering titleText))
          & colored titleColor (solidRectangle w titleHeight)
    titleText
      | T.length n > 10 = T.take 8 n <> "... = " <> formattedVal
      | otherwise = n <> " = " <> formattedVal
    formattedVal = pack (showFFloatAlt (Just 2) v "")
    collapseButton = rectangle 0.4 0.4 & solidPolygon [(-0.1, -0.1), (0.1, -0.1), (0, 0.1)]
    expandButton = rectangle 0.4 0.4 & solidPolygon [(-0.1, 0.1), (0.1, 0.1), (0, -0.1)]
    clientArea =
      picWhen (w * h > 0) $
        rectangle w h
          & clipped w h pic
          & colored bgColor (solidRectangle 5 1)

framedBounds :: FrameState -> Bounds
framedBounds (Parameter _ _ _ (left, top, w, h) _, (x, y), True, _) =
  (x + left, y + top + titleHeight, w, h + titleHeight)
framedBounds (Parameter _ _ _ (left, top, w, _) _, (x, y), False, _) =
  (x + left, y + top + titleHeight, w, titleHeight)

titleHeight :: Double
titleHeight = 0.7

untranslated :: Double -> Double -> Event -> Event
untranslated x y (PointerPress (px, py)) = PointerPress (px - x, py - y)
untranslated x y (PointerRelease (px, py)) = PointerRelease (px - x, py - y)
untranslated x y (PointerMovement (px, py)) = PointerMovement (px - x, py - y)
untranslated _ _ other = other

-- | Adjusts the output of a parameter by passing it through a conversion
-- function.  Built-in parameters usually range from 0 to 1, and conversions
-- can be used to rescale the output to a different range.
converted :: (Double -> Double) -> Parameter -> Parameter
converted c (Parameter name val pic bounds handle) =
  Parameter name (c val) pic bounds (converted c . handle)

-- | Changes the name of an existing parameter.
renamed :: Text -> Parameter -> Parameter
renamed name (Parameter _ val pic bounds handle) =
  Parameter name val pic bounds (renamed name . handle)

-- | A 'Parameter' with a constant value, and no way to change it.
constant :: Text -> Double -> Parameter
constant name n =
  parameterOf
    name
    n
    (const id)
    id
    (const blank)
    (const (-2.5, 0, 5, 0))

-- | Builder for 'Parameter' types that are clickable and 5x1 in size.
buttonOf ::
  Text ->
  state ->
  (state -> state) ->
  (state -> Double) ->
  (state -> Picture) ->
  Parameter
buttonOf name initial click value pic =
  parameterOf
    name
    (initial, False)
    change
    (value . fst)
    ( \(state, press) ->
        pic state
          & picWhen press (colored (RGBA 0 0 0 0.3) (solidRectangle 5 1))
    )
    (const (-2.5, 0.5, 5, 1))
  where
    change (PointerPress (px, py)) (state, _)
      | abs px < 2.5, abs py < 0.5 = (state, True)
    change (PointerRelease (px, py)) (state, True)
      | abs px < 2.5, abs py < 0.5 = (click state, False)
      | otherwise = (state, False)
    change _ (state, press) = (state, press)

-- | A 'Parameter' that can be toggled between 0 (off) and 1 (on).
toggle :: Text -> Parameter
toggle name = buttonOf name False not value picture
  where
    value True = 1
    value False = 0
    picture True = dilated 0.5 $ lettering "\x2611"
    picture False = dilated 0.5 $ lettering "\x2610"

-- | A 'Parameter' that counts how many times it has been clicked.
counter :: Text -> Parameter
counter name = buttonOf name 0 (+ 1) id picture
  where
    picture _ = dilated 0.5 (lettering "Next")

-- | A 'Parameter' that can be adjusted continuously between 0 and 1.
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

-- | A 'Parameter' that has a randomly chosen value.  It offers a button to
-- regenerate its value.
random :: Text -> Parameter
random name = buttonOf name initial (next . snd) fst picture
  where
    initial = next (unsafePerformIO newStdGen)
    picture _ = dilated 0.5 $ lettering "\x21ba Regenerate"
    next = randomR (0.0, 1.0)

-- | A 'Parameter' that changes over time. It can be paused or reset.
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

-- | A 'Parameter' that tracks the current hour, in local time.  The hour
-- is on a scale from 0 (meaning midnight) to 23 (meaning 11:00 pm).
currentHour :: Parameter
currentHour =
  parameterOf
    "hour"
    ()
    (const id)
    (\_ -> unsafePerformIO $ fromIntegral <$> todHour <$> getTimeOfDay)
    (const blank)
    (const (-2.5, 0, 5, 0))

-- | A 'Parameter' that tracks the current minute, in local time.  It
-- ranges from 0 to 59.
currentMinute :: Parameter
currentMinute =
  parameterOf
    "minute"
    ()
    (const id)
    (\_ -> unsafePerformIO $ fromIntegral <$> todMin <$> getTimeOfDay)
    (const blank)
    (const (-2.5, 0, 5, 0))

-- | A 'Parameter' that tracks the current second, in local time.  It
-- ranges from 0.0 up to (but not including) 60.0.  This includes
-- fractions of a second.  If that's not what you want, you can use
-- 'withConversion' to truncate the number.
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

picWhen :: Bool -> Picture -> Picture
picWhen True = id
picWhen False = const blank
