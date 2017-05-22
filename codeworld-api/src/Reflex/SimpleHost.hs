{-# LANGUAGE RankNTypes              #-}

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

-- | The interface to create a new reflex host ist a bit convoluted. Since we
-- have to do this twice (GHCJS and stand-alone driver), this module creates a
-- simpler interface for us to use.
module Reflex.SimpleHost (simpleReflexHost) where

import Data.IORef
import Control.Monad.Fix
import Data.Functor.Identity
import Data.Dependent.Sum (DSum ((:=>)))
import Reflex
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents, EventTrigger)

simpleReflexHost ::
    (forall t m . (Reflex t, MonadHold t m, MonadFix m)
              =>  (Event t a, Event t b)
              ->  (m (Behavior t c)))
    -> IO (a -> IO (), b -> IO (), IO c)
simpleReflexHost guest = runSpiderHost $ do
    (e1, e1TriggerRef) <- newEventWithTriggerRef
    (e2, e2TriggerRef) <- newEventWithTriggerRef
    b <- runHostFrame (guest (e1, e2))
    return
        ( sendEvent e1TriggerRef
        , sendEvent e2TriggerRef
        , runSpiderHost $ runHostFrame $ sample b
        )

sendEvent :: IORef (Maybe (EventTrigger Spider a)) -> a -> IO ()
sendEvent eventTriggerRef x =
    readIORef eventTriggerRef >>= mapM_ (\trig ->
            runSpiderHost $ fireEvents [trig :=> Identity x])

