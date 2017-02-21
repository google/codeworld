{-
  Copyright 2016-2017 The CodeWorld Authors. All rights reserved.

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
multi-player setup. It is the version of the code presented in the paper,
including here for completenss and testing.
-}

{-# LANGUAGE StandaloneDeriving #-}
module CodeWorld.Prediction.Paper where

import Data.List

type Player     =  Int
type Timestamp  =  Double
type Message    =  (Timestamp, Player, Event)
type Event      =  Char

class Game world where
  start ::  world
  step    ::  Double -> world -> world
  handle  ::  Player -> Event -> world -> world

gameRate :: Double
gameRate = 1/6

instance Eq world => Eq (Log world) where
    l1 == l2 = committed l1 == committed l2 &&
       sortMessages (events l1) == sortMessages (events l2) &&
       sort (latest l1) == sort (latest l2)
deriving instance Show world => Show (Log world)

-- Code from figure below

type TState world = (Timestamp, world)
data Log world = Log  {  committed  ::  TState world,
                         events     ::  [Message],
                         latest     ::  [(Player, Timestamp)] }

initLog :: Game world => [Player] -> Log world
initLog ps = Log (0, start) [] ([ (p,0) | p <- ps ])

addPing :: Game world => (Timestamp, Player) -> Log world -> Log world
addPing (t,p) log = recordActivity t p log

addEvent :: Game world => (Timestamp, Player, Event) -> Log world -> Log world
addEvent (t,p,e) log = recordActivity t p (log { events = events log ++ [(t,p,e)] })

recordActivity :: Game world => Timestamp -> Player -> Log world -> Log world
recordActivity t p log  | t <  t_old  = error "Messages out of order"
                        | otherwise      = advanceCommitted (log { latest = latest' })
  where  latest' = (p,t) : delete (p,t_old) (latest log)
         Just t_old = lookup p (latest log)

advanceCommitted :: Game world => Log world -> Log world
advanceCommitted log = log  {  events     = to_keep,
                               committed  = applyEvents to_commit (committed log) }
  where  (to_commit, to_keep) = partition (\(t,_,_) -> t < commitHorizon log) (events log)

commitHorizon :: Log world -> Timestamp
commitHorizon log = minimum [ t | (p,t) <- latest log ]

currentState :: Game world => Timestamp -> Log world -> world
currentState now log | now < commitHorizon log = error "Cannot look into the past"
currentState now log = gameStep (now - t) world
  where  (past_events, future_events) = partition (\(t,_,_) -> t <= now) (events log)
         (t, world) = applyEvents past_events (committed log)

applyEvents :: Game world => [Message] -> TState world -> TState world
applyEvents messages ts = foldl apply ts (sortMessages messages)
  where  apply (t0, world) (t1, p, e) = (t1, handle p e (gameStep (t1 - t0) world))

sortMessages :: [Message] -> [Message]
sortMessages = sortOn (\(t,p,_) -> (t,p))

gameStep :: Game world => Double -> world -> world
gameStep dt world  |  dt <= 0        =  world
                   |  dt > gameRate  =  gameStep (dt - gameRate) (step gameRate world)
                   |  otherwise      =  step dt world
