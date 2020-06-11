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

module Parameter
  {-# WARNING "This is an experimental API.  It can change at any time." #-}
  ( Parameter,
    parametricDrawingOf,
    slider,
    toggle,
    counter,
    constant,
    random,
    timer,
    currentHour,
    currentMinute,
    currentSecond,
    converted,
    renamed,
  )
where

import CodeWorld.Parameter
  ( Parameter,
    currentHour,
    currentMinute,
    currentSecond,
  )
import qualified CodeWorld.Parameter as CW
import Internal.Picture
import Internal.Text
import Prelude
import "base" Prelude ((.), map)

-- | A drawing that depends on parameters.  The first argument is a
-- list of parameters.  The second is a picture, which depends on the
-- values of those parameters.  Each number used to retrieve the picture
-- is the value of the corresponding parameter in the first list.
parametricDrawingOf :: ([Parameter], [Number] -> Picture) -> Program
parametricDrawingOf (params, pic) =
  CW.parametricDrawingOf params (toCWPic . pic . map fromDouble)

-- | Adjusts the output of a parameter by passing it through a conversion
-- function.  Built-in parameters usually range from 0 to 1, and conversions
-- can be used to rescale the output to a different range.
converted :: (Parameter, Number -> Number) -> Parameter
converted (p, c) = CW.converted (toDouble . c . fromDouble) p

-- | Changes the name of an existing parameter.
renamed :: (Parameter, Text) -> Parameter
renamed (p, name) = CW.renamed (fromCWText name) p

-- | A 'Parameter' with a constant value, and no way to change it.
constant :: (Text, Number) -> Parameter
constant (name, n) = CW.constant (fromCWText name) (toDouble n)

-- | A 'Parameter' that can be toggled between 0 (off) and 1 (on).
toggle :: Text -> Parameter
toggle name = CW.toggle (fromCWText name)

-- | A 'Parameter' that counts how many times it has been clicked.
counter :: Text -> Parameter
counter name = CW.counter (fromCWText name)

-- | A 'Parameter' that can be adjusted continuously between 0 and 1.
slider :: Text -> Parameter
slider name = CW.slider (fromCWText name)

-- | A 'Parameter' that has a randomly chosen value.  It offers a button to
-- regenerate its value.
random :: Text -> Parameter
random name = CW.random (fromCWText name)

-- | A 'Parameter' that changes over time. It can be paused or reset.
timer :: Text -> Parameter
timer name = CW.timer (fromCWText name)
