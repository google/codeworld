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
module CodeWorld.App3
    {-# WARNING "This is an experimental API.  It can change at any time." #-}
    (applicationOf) where

import CodeWorld.Driver
import CodeWorld.Event
import CodeWorld.Picture
import Control.Monad.Fix
import qualified Reflex as R

applicationOf
    :: (forall t m. (R.Reflex t, R.MonadHold t m, MonadFix m)
        => R.Event t Event -> m (R.Dynamic t Picture))
    -> IO ()
applicationOf = runReactive
