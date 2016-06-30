{-# LANGUAGE CPP                      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}

{-
  Copyright 2016 The CodeWorld Authors. All rights reserved.

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

module Internal.CodeWorld (
    Program,
    drawingOf,
    pictureOf,
    animationOf,
    simulationOf,
    interactionOf,
    traced
    ) where

import qualified "codeworld-api" CodeWorld as CW
import                           Data.Text (Text)
import                           Internal.Num (Number, fromDouble, toDouble)
import                           Internal.Picture
import                           Internal.Event
import qualified                 Internal.Text as CWT
import           "base"          Prelude
import                           System.IO.Unsafe
import                           System.Random

traced :: (a, CWT.Text) -> a
traced (x, msg) = CW.trace (CWT.fromCWText msg) x

type Program = IO ()

drawingOf :: Picture -> Program
drawingOf pic = CW.drawingOf (toCWPic pic)

pictureOf :: Picture -> Program
pictureOf pic = CW.pictureOf (toCWPic pic)
{-# WARNING pictureOf "Please use drawingOf(...) instead of pictureOf(...)" #-}

animationOf :: (Number -> Picture) -> Program
animationOf f = CW.animationOf (toCWPic . f . fromDouble)

simulationOf :: ([Number] -> world,
                 (world, Number) -> world,
                 world -> Picture)
             -> Program
simulationOf (initial, step, draw) = do
    rs <- chooseRandoms
    CW.simulationOf (initial rs)
                    (\dt w -> step (w, fromDouble dt))
                    (toCWPic . draw)

interactionOf :: ([Number] -> world,
                  (world, Number) -> world,
                  (world, Event) -> world,
                  world -> Picture)
              -> Program
interactionOf (initial, step, event, draw) = do
    rs <- chooseRandoms
    CW.interactionOf (initial rs)
                     (\dt w -> step (w, fromDouble dt))
                     (\ev w -> event (w, fromCWEvent ev))
                     (toCWPic . draw)

chooseRandoms :: IO [Number]
chooseRandoms = do
    n  <- randomRIO (0,1)
    ns <- unsafeInterleaveIO chooseRandoms
    return (fromDouble n : ns)
