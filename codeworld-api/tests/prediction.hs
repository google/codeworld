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

import CodeWorld.Prediction

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Bifunctor
import Data.List
import Text.Printf
import System.Exit

type Event = Int
type TimeStamp = Double
type TEvent = (TimeStamp, Maybe Event)

type EventsTodo = IM.IntMap [TEvent]
type EventsDone = IM.IntMap [TEvent]

type LogEntry = Either Double Event
type Log = [LogEntry]

step :: Double -> Log -> Log
step dt = (Left dt :)

handle :: Event -> Log -> Log
handle dt = (Right dt :)

rate :: Double
rate = 1/4 -- nicer decimal display

genEvents :: Int -> [TEvent]
genEvents player = map (first (fromIntegral player/8 +)) $
    [ (1, Nothing)
    , (2, Just player)
    , (3, Nothing)
    , (4, Just player)
    , (5, Just player)
    ]

schedule :: EventsTodo
schedule = IM.fromList [(0,genEvents 0), (1, genEvents 1)]

allDone :: [EventsDone]
allDone = traverse inits schedule -- wow!

type Check = (EventsDone, (Int, TEvent))
allChecks :: [Check]
allChecks = allDone >>= prevs

prevs :: EventsDone -> [Check]
prevs m = [ (IM.adjust init i m, (i, last done))
          | i <- IM.keys m
          , let done = m IM.! i
          , not (null done)
          ]

memo :: M.Map EventsDone (Future Log)
memo = M.fromList [ (eventsDone, recreate eventsDone) | eventsDone <- allDone ]

recreate :: EventsDone -> Future Log
recreate m = case prevs m of
    [] -> initFuture [] (IM.size m)
    (c:_) -> checkActual c

check :: Check -> Bool
check c = checkActual c `eqFuture` checkExpected c

checkActual (prev, (p,(t,e))) =
        currentTimePasses step rate t' $
        addEvent step rate p t (handle <$> e) $
        memo M.! prev
 where t' = max (fst (currentStateDirect (memo M.! prev))) t

checkExpected (prev, (p,(t,e))) = memo M.! IM.adjust (++[(t,e)]) p prev

failedChecks :: [Check]
failedChecks = filter (not . check) allChecks

reportFailedCheck :: Check -> IO ()
reportFailedCheck c = do
    putStrLn "Failed Check"
    putStrLn "History:"
    reportHistory (fst c)
    putStrLn "Event:"
    print (snd c)
    putStrLn "Before:"
    printInternalState showLog (memo M.! fst c)
    putStrLn "Expected:"
    printInternalState showLog (checkExpected c)
    putStrLn "Actual:"
    printInternalState showLog (checkActual c)
    putStrLn ""

reportHistory :: EventsDone -> IO ()
reportHistory m = mapM_ go $ IM.toList m
  where
    go (p, tes) = putStrLn $ show p ++ ": " ++ intercalate " " (map ste tes)
    ste (t, Nothing) = show t
    ste (t, Just e) = show e ++ "@" ++ show t

showLog :: Log -> String
showLog l = intercalate " " (map sle (reverse l))
  where
    sle (Left x)  = show x
    sle (Right x) = "["++show x++"]"

main = do
    mapM_ reportFailedCheck failedChecks
    if null failedChecks then exitSuccess else exitFailure
