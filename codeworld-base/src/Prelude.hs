{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
--------------------------------------------------------------------------------

-- | The standard set of functions and variables available to all programs.
--
--  You may use any of these functions and variables without defining them.
module Prelude
  ( module Internal.Exports,

    -- * Numbers
    module Internal.Num,

    -- * Text
    module Internal.Text,

    -- * General purpose functions
    module Internal.Prelude,
    IO,
  )
where

import Internal.CodeWorld
import Internal.Color
import Internal.Event
import Internal.Exports
import Internal.Num
import Internal.Picture
import Internal.Prelude hiding (randomsFrom)
import Internal.Text hiding (fromCWText, toCWText)
import "base" Prelude (IO)
