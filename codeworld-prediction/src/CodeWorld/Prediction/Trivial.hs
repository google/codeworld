{-
  Copyright 2017 The CodeWorld Authors. All rights reserved.

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

{- |
This module encapsulates the logics behind the prediction code in the
multi-player setup. It is the “trivially correct” version.
-}

{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module CodeWorld.Prediction.Trivial
    ( Timestamp, AnimationRate, StepFun, Future
    , initFuture, currentTimePasses, currentState, addEvent
    , eqFuture, printInternalState
    )
    where

import qualified Data.IntMap as IM
import qualified Data.MultiMap as M
import Data.Bifunctor (second)
import Data.List (foldl', intercalate)
import Text.Printf

type PlayerId = Int
type Timestamp = Double     -- in seconds, relative to some arbitrary starting point
type AnimationRate = Double -- in seconds, e.g. 0.1

-- All we do with events is to apply them to the state. So let's just store the
-- function that does that.
type Event s = s -> s

-- A state and an event only make sense together with a time.
type TState s = (Timestamp, s)
type TEvent s = (Timestamp, Event s)

type StepFun s = Double -> s -> s
type EventQueue s = M.MultiMap (Timestamp, PlayerId) (Event s)


-- | Invariants about the time stamps in this data type:
-- * committed <= pending <= current < future
-- * The time is advanced with strictly ascending timestamps
-- * For each player, events come in with strictly ascending timestamps
-- * For each player, all events in pending or future are before the
--   corresponding lastEvents entry.
data Future s = Future
    { initial :: s
    , events :: EventQueue s
    }

initFuture :: s -> Int -> Future s
initFuture s _numPlayers = Future
    { initial     = s
    , events      = M.empty
    }

-- Time handling.
--
-- Move state forward in fixed animation rate steps, and get
-- the timestamp as close to the given target as possible (but possibly stop short)
timePassesBigStep :: StepFun s -> AnimationRate -> Timestamp -> TState s -> TState s
timePassesBigStep step rate target (now, s)
    | now + rate <= target
    = timePassesBigStep step rate target (stepBy step rate (now, s))
    | otherwise
    = (now, s)

-- Move state forward in fixed animation rate steps, and get
-- the timestamp as close to the given target as possible, and then do a final small step
timePasses :: StepFun s -> AnimationRate -> Timestamp -> TState s -> TState s
timePasses step rate target
    = stepTo step target . timePassesBigStep step rate target

stepBy :: StepFun s -> Double -> TState s -> TState s
stepBy step diff (now,s)
    = (now + diff, step diff s)

stepTo :: StepFun s -> Timestamp -> TState s -> TState s
stepTo step target (now, s)
    = (target, step (target - now) s)

handleNextEvent :: StepFun s -> AnimationRate -> TEvent s -> TState s -> TState s
handleNextEvent step rate (target, event)
    = second event . timePasses step rate target

handleNextEvents :: StepFun s -> AnimationRate -> EventQueue s -> TState s -> TState s
handleNextEvents step rate eq ts
    = foldl' (flip (handleNextEvent step rate)) ts $
      map (\((t,_p),h) -> (t,h)) $
      M.toList eq

-- | This should be called shortly following 'currentTimePasses'
currentState :: StepFun s -> AnimationRate -> Timestamp -> Future s -> s
currentState step rate target f =
    snd $
    timePasses step rate target $
    handleNextEvents step rate to_apply (0, initial f)
  where
    (to_apply, _) = M.spanAntitone (\(t,p) -> t <= target) (events f)

-- | This should be called regularly, to keep the current state up to date,
-- and to incorporate future events in it.
currentTimePasses :: StepFun s -> AnimationRate -> Timestamp -> Future s -> Future s
currentTimePasses step rate target = id

-- | Take a new event into account, local or remote.
-- Invariant:
--  * The timestamp of the event is larger than the timestamp
--    of any event added for this player (which is the timestamp for the player
--    in `lastEvents`)
addEvent :: StepFun s -> AnimationRate ->
    PlayerId -> Timestamp -> Maybe (Event s) ->
    Future s -> Future s
  -- A future event.
addEvent step rate player now mbEvent f
    = f { events = maybe id (M.insertR (now, player)) mbEvent $ events f }

-- | Advances the current time (by big steps)
advanceCurrentTime :: StepFun s -> AnimationRate -> Timestamp -> Future s -> Future s
advanceCurrentTime step rate target = id

eqFuture :: Eq s => Future s -> Future s -> Bool
eqFuture f1 f2 = M.keys (events f1) == M.keys (events f2)

printInternalState :: (s -> String) -> Future s -> IO ()
printInternalState showState f = do
    printf "    Event keys: %s\n" (show (M.keys (events f)))

