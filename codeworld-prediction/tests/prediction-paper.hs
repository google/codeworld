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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}

import CodeWorld.Prediction.Paper

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Bifunctor
import Data.List
import System.Exit
import Test.QuickCheck

import Common hiding (Event)


type EventsTodo = (IM.IntMap [TEvent])
type EventsDone = (IM.IntMap [TEvent])

type LogEntry = Either Double Event
type Book = [LogEntry]

type Check = (EventsDone, (Int, TEvent))
type CheckReport = (Check, Log Book, Log Book, Log Book)

-- Fake state and handle functions

instance Game Book where
    start = []

    step :: Double -> Book -> Book
    step dt = (Left dt :)

    handle :: Player -> Event -> Book -> Book
    handle _ dt = (Right dt :)


-- Global constant
rate :: Double
rate = 1/4 -- decimal display


-- The actual check:
-- Exhaustively search the order in which these events could happen
-- Memoize every initial segment
-- Ensure that all possible ways reach the same conclusion.

failedChecks :: EventsTodo -> [CheckReport]
failedChecks schedule = map mkReport $ filter (not . check) allChecks
  where
    allDone :: [EventsDone]
    allDone = traverse inits schedule -- wow!

    allChecks :: [Check]
    allChecks = allDone >>= prevs

    prevs :: EventsDone -> [Check]
    prevs m =
        [ (IM.adjust init i m, (i, last done))
        | i <- IM.keys m
        , let done = m IM.! i
        , not (null done)
        ]

    memo :: M.Map EventsDone (Log Book)
    memo = M.fromList [ (eventsDone, recreate eventsDone) | eventsDone <- allDone ]

    recreate :: EventsDone -> Log Book
    recreate m = case prevs m of
        [] -> initLog (IM.keys m)
        (c:_) -> checkActual c

    check :: Check -> Bool
    check c = checkActual c == checkExpected c

    checkExpected :: Check -> Log Book
    checkExpected (prev, (p,(t,e))) = memo M.! IM.adjust (++[(t,e)]) p prev

    checkActual :: Check -> Log Book
    checkActual (prev, (p,(t,Nothing))) = addPing (t, p) $ memo M.! prev
    checkActual (prev, (p,(t,Just e))) = addEvent (t, p, e) $ memo M.! prev

    mkReport :: Check -> CheckReport
    mkReport c = (c, memo M.! fst c, checkExpected c, checkActual c)

-- The quickcheck test, with reporting

testPrediction :: Property
testPrediction = forAllSchedules $ \(_, schedule) ->
    let failed = failedChecks schedule
    in reportFailedCheck (head failed) `whenFail` null failed



reportFailedCheck :: CheckReport -> IO ()
reportFailedCheck (c, before, expected, actual) = do
    putStrLn "Failed Check"
    putStrLn "History:"
    putStr $ showHistory (fst c)
    putStrLn "Event:"
    print (snd c)
    putStrLn "Before:"
    print before
    putStrLn "Expected:"
    print expected
    putStrLn "Actual:"
    print actual
    putStrLn ""

showHistory :: EventsDone -> String
showHistory m = unlines $ map go (IM.toList m)
  where
    go (p, tes) = "Player " ++ show p ++ ": " ++ intercalate " " (map ste tes)
    ste (t, Nothing) = show t
    ste (t, Just e) = show e ++ "@" ++ show t

-- The main entry point.
-- Set the exit code to please Cabal.
main :: IO ()
main = do
    res <- quickCheckWithResult args testPrediction
    case res of
        Success {} -> exitSuccess
        _          -> exitFailure
  where
    args = stdArgs { maxSize = 30 } -- more gets too large
