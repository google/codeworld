{-# LANGUAGE GADTs #-}
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
  where

import CodeWorld
import Data.Text (pack)
import Data.Time.Clock
import Data.Time.LocalTime
import System.IO.Unsafe

type Conversion = Double -> Double

data Parameter where
    Parameter :: forall state. state
              -> Conversion
              -> (Event -> state -> state)
              -> (Conversion -> state -> Picture)
              -> (state -> Double)
              -> Parameter

withConversion :: Conversion -> Parameter -> Parameter
withConversion c1 (Parameter state c0 handler pic val) =
    Parameter state (c1 . c0) handler pic val

parametricDrawingOf :: [Parameter] -> ([Double] -> Picture) -> IO ()
parametricDrawingOf initialParams mainPic = do
    activityOf initialParams change picture
  where
    change event params = map (changeParam event) params
    picture params = pictures (map showParam params) & mainPic (map getParam params)

    changeParam event (Parameter state conv handler pic val) =
        Parameter (handler event state) conv handler pic val
    showParam (Parameter state conv _ pic _) = pic conv state
    getParam (Parameter state conv _ _ val) = conv (val state)

timer :: Parameter
timer = Parameter 0 id timeChange timePic id
  where timeChange (TimePassing dt) t = t + dt
        timeChange _ t = t
        timePic conv t = lettering (pack (show (conv t)))
                       & colored (light gray) (solidRectangle 10 1)

currentHour :: Parameter
currentHour = Parameter () id (const id) (\_ _ -> blank) get
  where get :: () -> Double
        get () = unsafePerformIO $ do
            now <- getCurrentTime
            timezone <- getCurrentTimeZone
            let zoneNow = utcToLocalTime timezone now
            let timeNow = localTimeOfDay zoneNow
            return (fromIntegral (todMin timeNow))

currentMinute :: Parameter
currentMinute = Parameter () id (const id) (\_ _ -> blank) get
  where get :: () -> Double
        get () = unsafePerformIO $ do
            now <- getCurrentTime
            timezone <- getCurrentTimeZone
            let zoneNow = utcToLocalTime timezone now
            let timeNow = localTimeOfDay zoneNow
            return (fromIntegral (todMin timeNow))

currentSecond :: Parameter
currentSecond = Parameter () id (const id) (\_ _ -> blank) get
  where get :: () -> Double
        get () = unsafePerformIO $ do
            now <- getCurrentTime
            timezone <- getCurrentTimeZone
            let zoneNow = utcToLocalTime timezone now
            let timeNow = localTimeOfDay zoneNow
            return (realToFrac (truncate (todSec timeNow) :: Integer))
