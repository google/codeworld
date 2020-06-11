{-# LANGUAGE PackageImports #-}

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

module Image where

import qualified "codeworld-api" CodeWorld.Image as CW
import GHC.Stack
import Internal.Num
import Internal.Picture
import Internal.Text
import "base" Prelude (($))

-- | @image(name, url, w, h)@ is an image from a standard image format.
--
-- * @name@ is a name for the picture, used for debugging.
-- * @url@ is a data-scheme URI for the image data.
-- * @w@ is the width in CodeWorld screen units.
-- * @h@ is the height in CodeWorld screen units.
--
-- The image can be any universally supported format, including SVG, PNG,
-- JPG, etc.  SVG should be preferred, as it behaves better with
-- transformations.
image :: HasCallStack => (Text, Text, Number, Number) -> Picture
image (name, url, w, h) =
  withFrozenCallStack $ CWPic $
    CW.image (fromCWText name) (fromCWText url) (toDouble w) (toDouble h)
