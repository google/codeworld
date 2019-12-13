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
import CodeWorld.Picture
import Data.Text (Text, pack)
import Data.Time.Clock
import Data.Time.LocalTime
import Numeric (showFFloatAlt)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (newStdGen, randomR)

type Conversion = Double -> Double

-- | A drawing that depends on parameters.  A parameter is a
parametricDrawingOf :: [Parameter] -> ([Double] -> Picture) -> IO ()
parametricDrawingOf initialParams mainPic =
  activityOf transformedParams change picture
  where
    transformedParams =
      [ paramTranslation x y
          $ paramResize 0.5
          $ p
        | p <- initialParams
        | x <- [-7.5, -2.5 ..],
          y <- [9, 7 .. -9]
      ]
    change event params = map (changeParam event) params
    picture params =
      pictures (map showParam params)
        & mainPic (map getParam params)
    changeParam event (Parameter handle _ _) = handle event
    showParam (Parameter _ val pic) = pic val
    getParam (Parameter _ val _) = val

data Parameter where
  Parameter ::
    (Event -> Parameter) ->
    Double ->
    (Double -> Picture) ->
    Parameter

parameterOf ::
  state ->
  (Event -> state -> state) ->
  (state -> Double) ->
  (Double -> state -> Picture) ->
  Parameter
parameterOf initial change value picture =
  Parameter
    (\e -> parameterOf (change e initial) change value picture)
    (value initial)
    (\v -> picture v initial)

paramConversion :: Conversion -> Parameter -> Parameter
paramConversion c (Parameter handle val pic) =
  Parameter (paramConversion c . handle) (c val) pic

paramTranslation :: Double -> Double -> Parameter -> Parameter
paramTranslation x y (Parameter handle val pic) =
  Parameter
    (paramTranslation x y . handle . untranslate)
    val
    (translated x y . pic)
  where
    untranslate (PointerPress (px, py)) = PointerPress (px - x, py - y)
    untranslate (PointerRelease (px, py)) = PointerRelease (px - x, py - y)
    untranslate (PointerMovement (px, py)) = PointerMovement (px - x, py - y)
    untranslate other = other

paramResize :: Double -> Parameter -> Parameter
paramResize k (Parameter handle val pic) =
  Parameter
    (paramResize k . handle . unsize)
    val
    (dilated k . pic)
  where
    unsize (PointerPress (px, py)) = PointerPress (px / k, py / k)
    unsize (PointerRelease (px, py)) = PointerRelease (px / k, py / k)
    unsize (PointerMovement (px, py)) = PointerMovement (px / k, py / k)
    unsize other = other

constant :: Text -> Double -> Parameter
constant name n = parameterOf n (const id) id picture
  where
    picture val _ = clipped 8 2 $
      lettering (name <> ": " <> pack (showFFloatAlt (Just 2) val ""))
        & rectangle 8 2
        & colored bgColor (solidRectangle 8 2)

toggle :: Text -> Parameter
toggle name = parameterOf False change value picture
  where
    change (PointerPress (px, py))
      | abs px < 4, abs py < 1 = not
    change _ = id
    value True = 1
    value False = 0
    picture val True = clipped 8 2 $
      lettering ("\x2611 " <> name <> ": " <> pack (showFFloatAlt (Just 2) val ""))
        & rectangle 8 2
        & colored bgColor (solidRectangle 8 2)
    picture val False =
      lettering ("\x2610 " <> name <> ": " <> pack (showFFloatAlt (Just 2) val ""))
        & rectangle 8 2
        & colored bgColor (solidRectangle 8 2)

slider :: Text -> Parameter
slider name = parameterOf (0.5, False) change fst picture
  where
    change (PointerPress (px, py)) (_, _)
      | abs px < 4, abs py < 1 = ((px + 3.5) / 7, True)
    change (PointerRelease _) (v, _) = (v, False)
    change (PointerMovement (px, _)) (_, True) =
      (min 1 $ max 0 $ (px + 4) / 8, True)
    change _ state = state
    picture val (raw, _) = clipped 8 3 $
      translated (-2.5) 0.5 (lettering name)
        & translated 2.5 0.5 (lettering (pack (showFFloatAlt (Just 2) val "")))
        & translated (raw * 7 - 3.5) (-0.5) (solidRectangle 0.25 1)
        & translated 0 (-0.5) (solidRectangle 7 0.2)
        & rectangle 8 3
        & colored bgColor (solidRectangle 8 3)

random :: Text -> Parameter
random name = parameterOf (next (unsafePerformIO newStdGen)) change value picture
  where
    change (PointerPress (px, py))
      | abs px < 4, abs py < 1 = next . snd
    change _ = id
    value = fst
    picture val _ = clipped 8 2 $
      lettering ("\x21ba " <> name <> ": " <> pack (showFFloatAlt (Just 2) val ""))
        & rectangle 8 2
        & colored bgColor (solidRectangle 8 2)
    next = randomR (0.0, 1.0)

timer :: Text -> Parameter
timer name = parameterOf (0, 1) change fst picture
  where
    change (TimePassing dt) (t, r) = (t + r * dt, r)
    change (PointerPress (px, py)) (t, r)
      | abs px < 4, abs py < 0.75 = (t, 1 - r)
    change _ state = state
    picture val (_, 0) = clipped 8 2 $
      lettering ("\x23e9 " <> name <> ": " <> pack (showFFloatAlt (Just 2) val ""))
        & rectangle 8 2
        & colored bgColor (solidRectangle 8 2)
    picture val _ = clipped 8 2 $
      lettering ("\x23f8 " <> name <> ": " <> pack (showFFloatAlt (Just 2) val ""))
        & rectangle 8 2
        & colored bgColor (solidRectangle 8 2)

currentHour :: Parameter
currentHour = parameterOf () (const id) value picture
  where
    value () = unsafePerformIO $ fromIntegral <$> todHour <$> getTimeOfDay
    picture val _ = clipped 8 2 $
      lettering ("hour: " <> pack (showFFloatAlt (Just 2) val ""))
        & rectangle 8 2
        & colored bgColor (solidRectangle 8 2)

currentMinute :: Parameter
currentMinute = parameterOf () (const id) value picture
  where
    value () = unsafePerformIO $ fromIntegral <$> todMin <$> getTimeOfDay
    picture val _ = clipped 8 2 $
      lettering ("minute: " <> pack (showFFloatAlt (Just 2) val ""))
        & rectangle 8 2
        & colored bgColor (solidRectangle 8 2)

currentSecond :: Parameter
currentSecond = parameterOf () (const id) value picture
  where
    value () = unsafePerformIO $ realToFrac <$> todSec <$> getTimeOfDay
    picture val _ = clipped 8 2 $
      lettering ("second: " <> pack (showFFloatAlt (Just 2) val ""))
        & rectangle 8 2
        & colored bgColor (solidRectangle 8 2)

getTimeOfDay :: IO TimeOfDay
getTimeOfDay = do
  now <- getCurrentTime
  timezone <- getCurrentTimeZone
  return (localTimeOfDay (utcToLocalTime timezone now))

bgColor :: Color
bgColor = RGB 0.8 0.85 0.95
