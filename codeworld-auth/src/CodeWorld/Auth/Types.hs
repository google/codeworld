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

{-|
Type aliases for simplify Snap type signatures etc.
-}

module CodeWorld.Auth.Types
    ( Route
    , SnapExcept
    ) where

import           Control.Monad.Trans.Except (ExceptT(..))
import           Data.ByteString (ByteString)
import           Snap.Core (Snap)

type SnapExcept a = ExceptT (Snap a) Snap a -> Snap a

type Route = (ByteString, Snap ())
