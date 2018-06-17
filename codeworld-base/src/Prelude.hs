{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

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
--------------------------------------------------------------------------------
-- |The standard set of functions and variables available to all programs.
--
-- You may use any of these functions and variables without defining them.
module Prelude (
    -- $intro
    -- * Numbers
      module Internal.Num
    -- * Text
    , module Internal.Text
    -- * General purpose functions
    , module Internal.Prelude
    , IO
    , module Internal.Exports
    ) where

import Internal.Exports
import "base" Prelude (IO)

import Internal.Num
import Internal.Prelude hiding (randomsFrom)
import Internal.Text hiding (fromCWText, toCWText)

import Internal.CodeWorld
import Internal.Color
import Internal.Event
import Internal.Picture

--------------------------------------------------------------------------------
-- $intro
-- Welome to CodeWorld!  You can define your own pictures, animations, and games
-- by defining variables and functions.  There are four kinds of CodeWorld
-- programs:
--
-- * Pictures.  To create a picture, you'll define the variable called @program@
--   using 'drawingOf'.  The parameter to 'drawingOf' should be a 'Picture'.
--   Example:
--
-- > program = drawingOf(tree)
--
-- * Animations.  To create an animation, you'll define the variable called
--   @program@ using 'animationOf'.  The parameter to 'animationOf' should be a
--   function, mapping each time in seconds (a 'Number') to a 'Picture' that is
--   shown at that time.  Example:
--
-- > program = animationOf(spinningWheel)
--
-- * Activities.  An activity is a program that can interact with the user by
--   responding to pointer and keyboard events.  It also has a persistent state
--   that can be used to remember information about the past.  To create an
--   activity, you should first decide on the type to describe the state of
--   things (called the "world" type).  You will then describe the activity with:
--   an initial world, a change function that describes how the world changes
--   when various things happen, and a picture function that converts the world
--   into a picture to display.  You will then use 'activityOf' to define
--   @program@.  Example:
--
-- > program = activityOf(initial, change, picture)
--
-- * Group activities.  Finally, you can build a multi-user activity that others
--   can join over the internet.  The process is similar to activities, except
--   that you'll specify a number of participants, and your change and picture
--   functions receive extra arguments describing which participant an event or
--   picture applies to.  You'll use 'groupActivityOf' to define these.
--   Example:
--
-- > program = groupActivityOf(n, initial, change, picture)
