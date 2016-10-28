{-
  Copyright 2016 The CodeWorld Authors. All rights reserved.

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
    ( Timestamp, AnimationRate, StepFun
    , Future
    , initFuture, timePasses, currentState
    , localEvent, serverEvent
    )
    where

import Data.Sequence (empty, Seq, (|>), drop)
import Data.Foldable (toList)

type Timestamp = Double     -- in seconds, relative to some arbitrary starting point
type AnimationRate = Double -- in seconds, e.g. 0.1

-- All we do with events is to apply them to the state. So let’s just store the
-- function that does that.
type Event s = s -> s

type StepFun s = Double -> s -> s
type PendingEvents s = Seq (Timestamp, Event s)

data Future s = Future
        { committed :: s
        , commitTime :: Timestamp
        , pending :: PendingEvents s
        , current :: s
        , currentTime :: Timestamp
        }

initFuture :: s -> Timestamp -> Future s
initFuture s now = Future
    { committed   = s
    , commitTime  = now
    , pending     = empty
    , current     = s
    , currentTime = now
    }

-- Time handling. Move forward in fixed animation rate steps, and get
-- currentTime close to the given time stamp (but possibly stop short)
timePasses :: StepFun s -> AnimationRate -> Timestamp -> Future s -> Future s
timePasses step rate now f@(Future {..})
    | now - currentTime > rate
    = timePasses step rate now (rateStep step rate f)
    | otherwise
    = f


rateStep :: StepFun s -> AnimationRate -> Future s -> Future s
rateStep step rate (Future {..})
    = Future { current = step rate current
             , currentTime = currentTime + rate
             , ..  }

commitedRateStep :: StepFun s -> AnimationRate -> Future s -> Future s
commitedRateStep step rate (Future {..})
    = Future { committed = step rate (committed)
             , commitTime = commitTime + rate
             , ..  }

-- this should be called after a call to timePasses, to avoid large steps
currentState :: StepFun s -> Timestamp -> Future s -> s
currentState step now (Future {..})
    = step (now - currentTime) current

localEvent :: StepFun s -> AnimationRate -> Timestamp -> Event s -> Future s -> Future s
localEvent step rate now event f@(Future {..})
    | now - currentTime > rate
    = localEvent step rate now event (rateStep step rate f)
    | otherwise
    = Future { pending = pending |> (now, event)
             , current = event . step (now - currentTime) $ current
             , currentTime = now
             , ..  }

serverEvent :: StepFun s -> AnimationRate -> Bool -> Timestamp -> Event s -> Future s -> Future s
serverEvent step rate ours now event f@(Future {..})
    | now - commitTime > rate
    = serverEvent step rate ours now event (commitedRateStep step rate f)
    | otherwise
    = let committed' = event . step (now - commitTime) $ committed
          commitTime' = now
          pending' = if ours then Data.Sequence.drop 1 pending else pending
          current' = replayPending step rate commitTime' (toList pending') currentTime committed'
      in Future { committed = committed'
                , commitTime = commitTime'
                , pending = pending'
                , current = current'
                , currentTime = currentTime }

replayPending :: StepFun s -> AnimationRate ->
    Timestamp -> [(Timestamp, Event s)] -> Timestamp ->
    s -> s
replayPending step rate from events@((ts,e):events') to
    | ts - from > rate
    = replayPending step rate (from + rate) events to . step rate
    | ts - from > 0
    = replayPending step rate ts events' to . e . step (ts - from)
    | otherwise -- event timestamp might be off, and might be in the past
    = replayPending step rate ts events' to . e
replayPending step rate from [] to
    | to - from > rate
    = replayPending step rate (from + rate) [] to . step rate
    | to - from > 0
    = step (to - from)
    | otherwise
    = id
