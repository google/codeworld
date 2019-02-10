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
{- |
This module encapsulates the logics behind the prediction code in the
multi-player setup.
-}
{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module CodeWorld.Prediction
    ( Timestamp
    , AnimationRate
    , StepFun
    , Future
    , initFuture
    , currentTimePasses
    , currentState
    , addEvent
    , currentStateDirect
    , eqFuture
    , printInternalState
    ) where

import Data.Bifunctor (second)
import qualified Data.IntMap as IM
import Data.List (foldl', intercalate)
import qualified Data.MultiMap as M
import Text.Printf

type PlayerId = Int

type Timestamp = Double -- in seconds, relative to some arbitrary starting point

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
    { committed :: TState s
    , lastQuery :: Timestamp
    , lastEvents :: IM.IntMap Timestamp
    , pending :: EventQueue s
    , future :: EventQueue s
    , current :: TState s
    }

initFuture :: s -> Int -> Future s
initFuture s numPlayers =
    Future
    { committed = (0, s)
    , lastQuery = 0
    , lastEvents = IM.fromList [(n, 0) | n <- [0 .. numPlayers - 1]]
    , pending = M.empty
    , future = M.empty
    , current = (0, s)
    }

-- Time handling.
--
-- Move state forward in fixed animation rate steps, and get
-- the timestamp as close to the given target as possible (but possibly stop short)
timePassesBigStep ::
       StepFun s -> AnimationRate -> Timestamp -> TState s -> TState s
timePassesBigStep step rate target (now, s)
    | now + rate <= target =
        timePassesBigStep step rate target (stepBy step rate (now, s))
    | otherwise = (now, s)

-- Move state forward in fixed animation rate steps, and get
-- the timestamp as close to the given target as possible, and then do a final small step
timePasses :: StepFun s -> AnimationRate -> Timestamp -> TState s -> TState s
timePasses step rate target =
    stepTo step target . timePassesBigStep step rate target

stepBy :: StepFun s -> Double -> TState s -> TState s
stepBy step diff (now, s) = (now + diff, step diff s)

stepTo :: StepFun s -> Timestamp -> TState s -> TState s
stepTo step target (now, s) = (target, step (target - now) s)

handleNextEvent ::
       StepFun s -> AnimationRate -> TEvent s -> TState s -> TState s
handleNextEvent step rate (target, event) =
    second event . timePasses step rate target

handleNextEvents ::
       StepFun s -> AnimationRate -> EventQueue s -> TState s -> TState s
handleNextEvents step rate eq ts =
    foldl' (flip (handleNextEvent step rate)) ts $
    map (\((t, _p), h) -> (t, h)) $ M.toList eq

-- | This should be called shortly following 'currentTimePasses'
currentState :: StepFun s -> AnimationRate -> Timestamp -> Future s -> s
currentState step rate target f = snd $ timePasses step rate target (current f)

-- | This should be called regularly, to keep the current state up to date,
-- and to incorporate future events in it.
currentTimePasses ::
       StepFun s -> AnimationRate -> Timestamp -> Future s -> Future s
currentTimePasses step rate target =
    advanceCurrentTime step rate target . advanceFuture step rate target

-- | Take a new event into account, local or remote.
-- Invariant:
--  * The timestamp of the event is larger than the timestamp
--    of any event added for this player (which is the timestamp for the player
--    in `lastEvents`)
addEvent ::
       StepFun s
    -> AnimationRate
    -> PlayerId
    -> Timestamp
    -> Maybe (Event s)
    -> Future s
    -> Future s-- A future event.
addEvent step rate player now mbEvent f
    | now > lastQuery f =
        recordActivity step rate player now $
        f {future = maybe id (M.insertR (now, player)) mbEvent $ future f}
  -- A past event, goes to pending events. Pending events need to be replayed.
addEvent step rate player now mbEvent f =
    replayPending step rate $
    recordActivity step rate player now $
    f {pending = maybe id (M.insertR (now, player)) mbEvent $ pending f}

-- | Updates the 'lastEvents' field, and possibly updates the commmitted state
recordActivity ::
       StepFun s
    -> AnimationRate
    -> PlayerId
    -> Timestamp
    -> Future s
    -> Future s
recordActivity step rate player now f =
    advanceCommitted step rate $
    f {lastEvents = IM.insert player now $ lastEvents f}

-- | Commits events from the pending queue that are past the commitTime
advanceCommitted :: StepFun s -> AnimationRate -> Future s -> Future s
advanceCommitted step rate f
    | M.null eventsToCommit = f -- do not bother
    | otherwise = f {committed = committed', pending = uncommited'}
  where
    commitTime' = minimum $ IM.elems $ lastEvents f
    canCommit t = t < commitTime'
    (eventsToCommit, uncommited') = M.spanAntitone (canCommit . fst) (pending f)
    committed' = handleNextEvents step rate eventsToCommit $ committed f

-- | Throws away the current state, and recreates it from
--   pending events. To be used when inserting a pending event.
replayPending :: StepFun s -> AnimationRate -> Future s -> Future s
replayPending step rate f = f {current = current'}
  where
    current' =
        timePassesBigStep step rate (lastQuery f) $
        handleNextEvents step rate (pending f) $ committed f

-- | Takes into account all future event that happen before the given 'Timestamp'
--   Does not have to call 'replayPending', as we only append to the 'pending' queue.
--   But does have to call 'advanceCommitted', as the newly added events might
--   go directly to the committed state.
advanceFuture :: StepFun s -> AnimationRate -> Timestamp -> Future s -> Future s
advanceFuture step rate target f
    | M.null toPerform = f
    | otherwise =
        advanceCommitted step rate $
        f {current = current', pending = pending', future = future'}
  where
    hasHappened t = t <= target
    (toPerform, future') = M.spanAntitone (hasHappened . fst) (future f)
    pending' = pending f `M.union` toPerform
    current' = handleNextEvents step rate toPerform $ current f

-- | Advances the current time (by big steps)
advanceCurrentTime ::
       StepFun s -> AnimationRate -> Timestamp -> Future s -> Future s
advanceCurrentTime step rate target f =
    f
    { current = timePassesBigStep step rate target $ current f
    , lastQuery = target
    }

-- | Only for testing.
currentStateDirect :: Future s -> (Timestamp, s)
currentStateDirect = current

-- | Only for testing.
eqFuture :: Eq s => Future s -> Future s -> Bool
eqFuture f1 f2 =
    ( current f1
    , lastEvents f1
    , M.keys (pending f1)
    , M.keys (future f1)
    , committed f1) ==
    ( current f2
    , lastEvents f2
    , M.keys (pending f2)
    , M.keys (future f2)
    , committed f2)

-- | Only for testing.
printInternalState :: (s -> String) -> Future s -> IO ()
printInternalState showState f = do
    printf "   Current: (%6.3f) %s\n" (fst (current f)) $
        showState (snd (current f))
    printf "    Latest: %s\n" $
        intercalate
            " "
            [printf "%d:%.3f" p t | (p, t) <- IM.toList (lastEvents f)]
    printf " Committed: (%6.3f) %s\n" (fst (committed f)) $
        showState (snd (committed f))
