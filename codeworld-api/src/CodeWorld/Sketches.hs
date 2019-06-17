{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

module CodeWorld.Sketches (
    sketchedBoy
    ) where

import CodeWorld.Picture
import GHC.Stack
import Util.EmbedAsUrl

sketchedBoy :: HasCallStack => Picture
sketchedBoy = Sketch (getDebugSrcLoc callStack)
                     "sketchedBoy"
                     $(embedAsUrl "image/svg+xml" "data/BoyTransparency70.svg")
