{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-name-shadowing #-}

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

module CodeWorld.App
  {-# WARNING "This is an experimental API.  It can change at any time." #-}
  ( Rule,
    eventRule,
    pictureRule,
    multiEventRule,
    multiPictureRule,
    subrule,
    rules,
    applicationOf,
    unsafeMultiApplicationOf,
  )
where

import CodeWorld
import Data.List (foldl')
import System.Random (StdGen)

data Rule :: * -> * where
  EventRule :: (Int -> Event -> state -> state) -> Rule state
  PictureRule :: (Int -> state -> Picture) -> Rule state
  Rules :: [Rule state] -> Rule state

eventRule :: (Event -> state -> state) -> Rule state
eventRule = EventRule . const

pictureRule :: (state -> Picture) -> Rule state
pictureRule = PictureRule . const

multiEventRule :: (Int -> Event -> state -> state) -> Rule state
multiEventRule = EventRule

multiPictureRule :: (Int -> state -> Picture) -> Rule state
multiPictureRule = PictureRule

subrule :: (a -> b) -> (b -> a -> a) -> Rule b -> Rule a
subrule getter setter (EventRule event_b) = EventRule event_a
  where
    event_a k ev a = setter (event_b k ev (getter a)) a
subrule getter _setter (PictureRule pic_b) = PictureRule pic_a
  where
    pic_a n = pic_b n . getter
subrule getter setter (Rules rules) = Rules (map (subrule getter setter) rules)

rules :: [Rule state] -> Rule state
rules = Rules

applicationOf :: world -> [Rule world] -> IO ()
applicationOf w rules = activityOf w event picture
  where
    event ev = foldl' (.) id [f ev | f <- concatMap eventHandlers rules]
    picture w = pictures [pic w | pic <- concatMap pictureHandlers rules]
    eventHandlers (EventRule f) = [f 0]
    eventHandlers (Rules rs) = concatMap eventHandlers rs
    eventHandlers _ = []
    pictureHandlers (PictureRule f) = [f 0]
    pictureHandlers (Rules rs) = concatMap pictureHandlers rs
    pictureHandlers _ = []

unsafeMultiApplicationOf :: Int -> (StdGen -> state) -> [Rule state] -> IO ()
unsafeMultiApplicationOf n initial rules =
  unsafeGroupActivityOf n initial event picture
  where
    event k ev = foldl' (.) id [f k ev | f <- concatMap eventHandlers rules]
    picture k w = pictures [pic k w | pic <- concatMap pictureHandlers rules]
    eventHandlers (EventRule f) = [f]
    eventHandlers (Rules rs) = concatMap eventHandlers rs
    eventHandlers _ = []
    pictureHandlers (PictureRule f) = [f]
    pictureHandlers (Rules rs) = concatMap pictureHandlers rs
    pictureHandlers _ = []
