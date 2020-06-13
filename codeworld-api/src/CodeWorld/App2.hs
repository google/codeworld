{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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

module CodeWorld.App2
  {-# WARNING "This is an experimental API.  It can change at any time." #-}
  ( Application,
    defaultApplication,
    withEventHandler,
    withPicture,
    withMultiEventHandler,
    withMultiPicture,
    subapplication,
    applicationOf,
  )
where

import CodeWorld

data Application :: * -> * where
  App ::
    state ->
    (Int -> Event -> state -> state) ->
    (Int -> state -> Picture) ->
    Application state

defaultApplication :: state -> Application state
defaultApplication s =
  App s (const (const id)) (const (const blank))

withEventHandler ::
  (Event -> state -> state) ->
  Application state ->
  Application state
withEventHandler f (App initial event picture) =
  App initial (\k ev -> f ev . event k ev) picture

withPicture :: (state -> Picture) -> Application state -> Application state
withPicture f (App initial event picture) =
  App initial event (\k s -> f s & picture k s)

withMultiEventHandler ::
  (Int -> Event -> state -> state) ->
  Application state ->
  Application state
withMultiEventHandler f (App initial event picture) =
  App initial (\k ev -> f k ev . event k ev) picture

withMultiPicture ::
  (Int -> state -> Picture) ->
  Application state ->
  Application state
withMultiPicture f (App initial event picture) =
  App initial event (\k s -> f k s & picture k s)

subapplication ::
  (a -> b) ->
  (b -> a -> a) ->
  Application b ->
  (b -> a) ->
  Application a
subapplication getter setter (App initial event picture) f =
  App
    (f initial)
    (\k ev s -> setter (event k ev (getter s)) s)
    (\k -> picture k . getter)

applicationOf :: Application world -> IO ()
applicationOf (App initial event picture) =
  activityOf initial (event 0) (picture 0)
