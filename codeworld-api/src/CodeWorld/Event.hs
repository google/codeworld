{-# LANGUAGE PatternSynonyms #-}

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
module CodeWorld.Event where

import CodeWorld.Picture (Point)
import Data.Text (Text)

{-| An event initiated by the user.

    Values of this type represent events that the user triggers when
    using an interactive program.

    Key events describe the key as 'Text'.  Most keys are represented
    by a single character text string, with the capital letter or other
    symbol from the key.  Keys that don't correspond to a single
    character use longer names from the following list.  Keep in mind
    that not all of these keys appear on all keyboards.

    * Up, Down, Left, and Right for the cursor keys.
    * F1, F2, etc. for function keys.
    * Backspace
    * Tab
    * Enter
    * Shift
    * Ctrl
    * Alt
    * Esc
    * PageUp
    * PageDown
    * End
    * Home
    * Insert
    * Delete
    * CapsLock
    * NumLock
    * ScrollLock
    * PrintScreen
    * Break
    * Separator
    * Cancel
    * Help
-}
data Event
    = KeyPress !Text
    | KeyRelease !Text
    | MousePress !MouseButton
                 !Point
    | MouseRelease !MouseButton
                   !Point
    | MouseMovement !Point
    | TimePassing !Double
    deriving (Eq, Show, Read)

data MouseButton
    = LeftButton
    | MiddleButton
    | RightButton
    deriving (Eq, Show, Read)

pattern PointerPress :: Point -> Event
pattern PointerPress p = MousePress LeftButton p

pattern PointerRelease :: Point -> Event
pattern PointerRelease p = MouseRelease LeftButton p

pattern PointerMovement :: Point -> Event
pattern PointerMovement p = MouseMovement p

{-# WARNING MousePress "Please use PointerPress instead of MousePress; MousePress will be removed July 2019." #-}
{-# WARNING MouseRelease "Please use PointerRelease instead of MouseRelease; MouseRelease will be removed July 2019." #-}
{-# WARNING MouseMovement "Please use PointerMovement instead of MouseMovement; MouseMovement will be removed July 2019." #-}
{-# WARNING MouseButton "Please use pointer events, which don't have buttons; MouseButton will be removed July 2019." #-}
