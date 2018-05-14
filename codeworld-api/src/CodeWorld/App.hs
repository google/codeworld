{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}

{-
  Copyright 2018 The CodeWorld Authors. All rights reserved.

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

module CodeWorld.App (
    Rule,
    timeRule,
    eventRule,
    pictureRule,
    multiEventRule,
    multiPictureRule,
    rules,
    applicationOf,
    unsafeMultiApplicationOf
    ) where

import CodeWorld
import Data.List (foldl')
import System.Random (StdGen)

data Rule :: * -> * where
    TimeRule :: (Double -> state -> state) -> Rule state
    EventRule :: (Event -> state -> state) -> Rule state
    PictureRule :: (state -> Picture) -> Rule state
    MultiEventRule :: (Int -> Event -> state -> state) -> Rule state
    MultiPictureRule :: (Int -> state -> Picture) -> Rule state
    Rules :: [Rule state] -> Rule state

timeRule :: (Double -> state -> state) -> Rule state
timeRule = TimeRule

eventRule :: (Event -> state -> state) -> Rule state
eventRule = EventRule

pictureRule :: (state -> Picture) -> Rule state
pictureRule = PictureRule

multiEventRule :: (Int -> Event -> state -> state) -> Rule state
multiEventRule = MultiEventRule

multiPictureRule :: (Int -> state -> Picture) -> Rule state
multiPictureRule = MultiPictureRule

rules :: [Rule state] -> Rule state
rules = Rules

applicationOf :: world -> [Rule world] -> IO ()
applicationOf w rules = interactionOf w step event picture
  where step dt   = foldl' (.) id [ f dt | f <- concatMap stepHandlers rules ]
        event ev  = foldl' (.) id [ f ev | f <- concatMap eventHandlers rules ]
        picture w = pictures [ pic w | pic <- concatMap pictureHandlers rules ]

        stepHandlers (TimeRule f)            = [f]
        stepHandlers (Rules rs)              = concatMap stepHandlers rs
        stepHandlers _                       = []

        eventHandlers (EventRule f)          = [f]
        eventHandlers (MultiEventRule f)     = [f 0]
        eventHandlers (Rules rs)             = concatMap eventHandlers rs
        eventHandlers _                      = []

        pictureHandlers (PictureRule f)      = [f]
        pictureHandlers (MultiPictureRule f) = [f 0]
        pictureHandlers (Rules rs)           = concatMap pictureHandlers rs
        pictureHandlers _                    = []

unsafeMultiApplicationOf :: Int -> (StdGen -> state) -> [Rule state] -> IO ()
unsafeMultiApplicationOf n initial rules =
    unsafeCollaborationOf n initial step event picture
  where step dt     = foldl' (.) id [ f dt | f <- concatMap stepHandlers rules ]
        event k ev  = foldl' (.) id [ f k ev | f <- concatMap eventHandlers rules ]
        picture k w = pictures [ pic k w | pic <- concatMap pictureHandlers rules ]

        stepHandlers (TimeRule f)            = [f]
        stepHandlers (Rules rs)              = concatMap stepHandlers rs
        stepHandlers _                       = []

        eventHandlers (EventRule f)          = [const f]
        eventHandlers (MultiEventRule f)     = [f]
        eventHandlers (Rules rs)             = concatMap eventHandlers rs
        eventHandlers _                      = []

        pictureHandlers (PictureRule f)      = [const f]
        pictureHandlers (MultiPictureRule f) = [f]
        pictureHandlers (Rules rs)           = concatMap pictureHandlers rs
        pictureHandlers _                    = []
