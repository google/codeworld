{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}

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

module CodeWorld.App2
    {-# WARNING "This is an experimental API.  It can change at any time." #-}
    (
    Application,
    defaultApplication,
    withTimeStep,
    withEventHandler,
    withPicture,
    withMultiEventHandler,
    withMultiPicture,
    subapplication,
    applicationOf
    ) where

import CodeWorld
import Data.List (foldl')
import System.Random (StdGen)

data Application :: * -> * where
    App :: state
        -> (Double -> state -> state)
        -> (Int -> Event -> state -> state)
        -> (Int -> state -> Picture)
        -> Application state

defaultApplication :: state -> Application state
defaultApplication s =
    App s (const id) (const (const id)) (const (const blank))

withTimeStep :: (Double -> state -> state)
             -> Application state
             -> Application state
withTimeStep f (App initial step event picture) =
    App initial (\dt -> f dt . step dt) event picture

withEventHandler :: (Event -> state -> state)
                 -> Application state
                 -> Application state
withEventHandler f (App initial step event picture) =
    App initial step (\k ev -> f ev . event k ev) picture

withPicture :: (state -> Picture) -> Application state -> Application state
withPicture f (App initial step event picture) =
    App initial step event (\k s -> f s & picture k s)

withMultiEventHandler :: (Int -> Event -> state -> state)
                      -> Application state
                      -> Application state
withMultiEventHandler f (App initial step event picture) =
    App initial step (\k ev -> f k ev . event k ev) picture

withMultiPicture :: (Int -> state -> Picture)
                 -> Application state
                 -> Application state
withMultiPicture f (App initial step event picture) =
    App initial step event (\k s -> f k s & picture k s)

subapplication :: (a -> b)
               -> (b -> a -> a)
               -> Application b
               -> (b -> a)
               -> Application a
subapplication getter setter (App initial step event picture) f =
    App (f initial)
        (\dt s   -> setter (step dt (getter s)) s)
        (\k ev s -> setter (event k ev (getter s)) s)
        (\k      -> picture k . getter)

applicationOf :: Application world -> IO ()
applicationOf (App initial step event picture) =
    interactionOf initial step (event 0) (picture 0)
