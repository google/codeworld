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

data Parameter where
    Parameter :: forall state. state
              -> (Event -> state -> state)
              -> (state -> Picture)
              -> (state -> Double)
              -> Parameter

parametricDrawingOf :: [Parameter] -> ([Double] -> Picture) -> IO ()
parametricDrawingOf initialParams mainPic = do
    activityOf initialParams change picture
  where
    change event params = map (changeParam event) params
    picture params = pictures (map showParam params) & mainPic (map getParam params)

    changeParam event (Parameter state handler pic val) =
        Parameter (handler event state) handler pic val
    showParam (Parameter state _ pic _) = pic state
    getParam (Parameter state _ _ val) = val state

timer :: Parameter
timer = Parameter 0 timeChange timePic id
  where timeChange (TimePassing dt) t = t + dt
        timeChange _ t = t
        timePic t = lettering (pack (show t)) & colored (light gray) (solidRectangle 10 1)

currentMinute :: Parameter
currentMinute = Parameter () (const id) (const blank) get
  where get :: () -> Double
        get () = unsafePerformIO $ do
            now <- getCurrentTime
            timezone <- getCurrentTimeZone
            let zoneNow = utcToLocalTime timezone now
            let timeNow = localTimeOfDay zoneNow
            return (fromIntegral (todMin timeNow))

currentSecond :: Parameter
currentSecond = Parameter () (const id) (const blank) get
  where get :: () -> Double
        get () = unsafePerformIO $ do
            now <- getCurrentTime
            timezone <- getCurrentTimeZone
            let zoneNow = utcToLocalTime timezone now
            let timeNow = localTimeOfDay zoneNow
            return (realToFrac (truncate (todSec timeNow) :: Integer))
