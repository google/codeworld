{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
module Internal.CodeWorld
  ( Program,
    drawingOf,
    animationOf,
    activityOf,
    debugActivityOf,
    groupActivityOf,
    simulationOf,
    debugSimulationOf,
    interactionOf,
    debugInteractionOf,
    collaborationOf,
    traced,
  )
where

import qualified "codeworld-api" CodeWorld as CW
import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T
import ErrorSanitizer
import Internal.Event
import Internal.Num (Number, fromDouble, fromInt, toDouble, toInt)
import Internal.Picture
import Internal.Prelude (randomsFrom)
import qualified Internal.Text as CWT
import System.IO
import System.Random
import "base" Prelude

data LiteralException = LiteralException Text

instance Exception LiteralException

instance Show LiteralException where
  show (LiteralException msg) = T.unpack (rewriteErrors msg)

traced :: (a, CWT.Text) -> a
traced (x, msg) = CW.trace (CWT.fromCWText msg) x

-- | A computer program.
--
-- This is a specific task or application that a computer performs.  In
-- CodeWorld, programs are defined using one of the "entry point"
-- functions, such as 'drawingOf', 'animationOf', or 'activityOf'.
type Program = IO ()

-- | A program that displays a drawing of a single picture.
--
-- This is the simplest way to define a program in CodeWorld.  The
-- argument is a 'Picture'.
--
-- For example:
--
-- > program = drawingOf(codeWorldLogo)
drawingOf :: Picture -> Program
drawingOf pic = CW.drawingOf (toCWPic pic) `catch` reportError

-- | A program that shows an animation changing over time.
--
-- The argument is a function, which maps each time (a 'Number' in
-- seconds since the program started) to the 'Picture' that is shown
-- at that time.
--
-- For example:
--
-- > program = animationOf(f)
-- > f(t) = rotated(rectangle(5,5), 45 * t)
animationOf :: (Number -> Picture) -> Program
animationOf f = CW.animationOf (toCWPic . f . fromDouble) `catch` reportError

-- | A program that can interact with a user by responding to pointer
-- and keyboard events and remember state.
--
-- To create an activity, you first choose a type for its state,
-- called the "world" type.  You will then describe the activity
-- with three arguments:
--
-- 1. A function to create an initial world.  The argument to
--    this function is an infinite sequence of random numbers
--    (chosen uniformly between 0 and 1) which you can use to
--    create a different world each time your program is run.
-- 2. A function that describes how the world changes when
--    things happen.  The function receives an old world and
--    an 'Event' that occurs, and maps it to a new world.
-- 3. A function that converts a world into a 'Picture', used
--    to display the program on the screen.
--
-- For example:
--
-- > program = activityOf(initial, change, picture)
-- >
-- > initial(randoms) = x0
-- >   where x0 = 10 * randoms#1 - 5
-- >
-- > change(x, KeyPress("A")) = x - 1
-- > change(x, KeyPress("D")) = x + 1
-- > change(x, other) = x
-- >
-- > picture(x) = translated(solidCircle(1), x, 0)
--
-- In mathematical language, an activity is called a dynamical
-- system.  The world type is known as the phase space of the
-- system, and the @change@ function is called the dynamics of
-- the system.  The @initial@ and @picture@ functions are not
-- part of the system, but describe how the program should
-- simulate and display it.
activityOf ::
  ( [Number] -> world,
    (world, Event) -> world,
    world -> Picture
  ) ->
  Program
activityOf (initial, event, draw) = interactionOf (initial, fst, event, draw)

-- | A program that acts just like one built with 'activityOf',
-- but offers more tools to pause, rewind, and otherwise
-- understand the program behavior.  This can be used during
-- development, and then changed back to 'activityOf' once the
-- program is complete.
debugActivityOf ::
  ( [Number] -> world,
    (world, Event) -> world,
    world -> Picture
  ) ->
  Program
debugActivityOf (initial, event, draw) =
  debugInteractionOf (initial, fst, event, draw)

-- | A program that interacts with multiple different users by
-- responding to their pointer and keyboard events.
--
-- The arguments to this function are similar 'activityOf',
-- except that:
--
-- 1. A first argument, a 'Number' gives the desired number of
--    players.
-- 2. The @change@ function receives an extra argument telling
--    which player intiated the event.
-- 3. The @picture@ function receives an extra argument with
--    the player for whom the picture should be built.
--
-- The activity will always begin with a "lobby", where players
-- can create new games or join existing games with a code.
-- Once the desired number of players have joined, the activity
-- will begin.
groupActivityOf ::
  ( Number,
    [Number] -> state,
    (state, Event, Number) -> state,
    (state, Number) -> Picture
  ) ->
  Program
groupActivityOf (players, initial, event, picture) =
  collaborationOf (players, initial, fst, event, picture)

simulationOf ::
  ([Number] -> world, (world, Number) -> world, world -> Picture) ->
  Program
simulationOf (initial, step, draw) =
  do
    rs <- chooseRandoms
    CW.simulationOf
      (initial rs)
      (\dt w -> step (w, fromDouble dt))
      (toCWPic . draw)
    `catch` reportError
{-# WARNING
  simulationOf
  [ "Please use activityOf instead of simulationOf.",
    "simulationOf may be removed July 2020."
  ]
  #-}

debugSimulationOf ::
  ([Number] -> world, (world, Number) -> world, world -> Picture) ->
  Program
debugSimulationOf (initial, step, draw) =
  do
    rs <- chooseRandoms
    CW.debugSimulationOf
      (initial rs)
      (\dt w -> step (w, fromDouble dt))
      (toCWPic . draw)
    `catch` reportError
{-# WARNING
  debugSimulationOf
  [ "Please use debugActivityOf instead of debugSimulationOf.",
    "debugSimulationOf may be removed July 2020."
  ]
  #-}

interactionOf ::
  ( [Number] -> world,
    (world, Number) -> world,
    (world, Event) -> world,
    world -> Picture
  ) ->
  Program
interactionOf (initial, step, event, draw) =
  do
    rs <- chooseRandoms
    CW.interactionOf
      (initial rs)
      (\dt w -> step (w, fromDouble dt))
      (\ev w -> event (w, fromCWEvent ev))
      (toCWPic . draw)
    `catch` reportError
{-# WARNING
  interactionOf
  [ "Please use activityOf instead of interactionOf.",
    "interactionOf may be removed July 2020."
  ]
  #-}

debugInteractionOf ::
  ( [Number] -> world,
    (world, Number) -> world,
    (world, Event) -> world,
    world -> Picture
  ) ->
  Program
debugInteractionOf (initial, step, event, draw) =
  do
    rs <- chooseRandoms
    CW.debugInteractionOf
      (initial rs)
      (\dt w -> step (w, fromDouble dt))
      (\ev w -> event (w, fromCWEvent ev))
      (toCWPic . draw)
    `catch` reportError
{-# WARNING
  debugInteractionOf
  [ "Please use debugActivityOf instead of debugInteractionOf.",
    "debugInteractionOf may be removed July 2020."
  ]
  #-}

collaborationOf ::
  ( Number,
    [Number] -> state,
    (state, Number) -> state,
    (state, Event, Number) -> state,
    (state, Number) -> Picture
  ) ->
  Program
collaborationOf (players, initial, step, event, picture) =
  -- This is safe ONLY because codeworld-base does not export the
  -- IO combinators that allow for choosing divergent clients.
  CW.unsafeCollaborationOf
    (toInt players)
    (initial . randomsFrom)
    (\dt state -> step (state, fromDouble dt))
    (\player ev state -> event (state, fromCWEvent ev, fromInt player + 1))
    (\player state -> toCWPic (picture (state, fromInt player + 1)))
    `catch` reportError
{-# WARNING
  collaborationOf
  [ "Please use groupActivityOf instead of collaborationOf.",
    "collaborationOf may be removed July 2020."
  ]
  #-}

chooseRandoms :: IO [Number]
chooseRandoms = do
  g <- newStdGen
  return (map fromDouble (randomRs (0, 1) g))

reportError :: SomeException -> IO ()
reportError ex = throwIO (LiteralException (T.pack (show ex)))
