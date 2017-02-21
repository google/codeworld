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
import Test.QuickCheck

type Event = Int
type TimeStamp = Double
type TEvent = (TimeStamp, Maybe Event)

type EventsTodo = ([TimeStamp], IM.IntMap [TEvent])
type EventsDone = ([TimeStamp], IM.IntMap [TEvent])

type LogEntry = Either Double Event
type Log = [LogEntry]

type Check = (EventsDone, Either Double (Int, TEvent))
type CheckReport = (Check, Future Log, Future Log, Future Log)

-- Fake state and handle functions

step :: Double -> Log -> Log
step dt = (Left dt :)

handle :: Event -> Log -> Log
handle dt = (Right dt :)

-- Global constant
rate :: Double
rate = 1/4 -- decimal display


-- Generation of random schedules

newtype GenEventsTodo = GenEventsTodo EventsTodo

instance Arbitrary GenEventsTodo where
    arbitrary = do
        -- get ascending positive timestamps
        tss  <- map getPositive . getOrdered <$> arbitrary
        p1ts <- map getPositive . getOrdered <$> arbitrary
        p2ts <- map getPositive . getOrdered <$> arbitrary
        -- some are just pings, some are real events
        p1 <- traverse (addEvent 0) p1ts
        p2 <- traverse (addEvent 1) p2ts
        return $ GenEventsTodo (tss, IM.fromList [(0,p1), (1,p2)])
      where addEvent i ts = do
               coin <- arbitrary
               if coin then return $ (ts, Nothing)
                       else return $ (ts, Just i)

    -- shrinking removes poitns of the list, and
    -- further reduces singleton lists mildly
    -- (the original shrink would get us too close to epsilon)
    shrink (GenEventsTodo (tss,m)) = map GenEventsTodo $ unShrinkOne $
        ((,) <$> listShrinkOne' mildlySmaller tss
             <*> traverse (listShrinkOne' (firstA mildlySmaller)) m)
      where mildlySmaller x | x > 1     = [x - 1]
                            | otherwise = []

-- The actual check:
-- Exhaustively search the order in which these events could happen
-- Memoize every initial segment
-- Ensure that all possible ways reach the same conclusion.

failedChecks :: EventsTodo -> [CheckReport]
failedChecks schedule = map mkReport $ filter (not . check) allChecks
  where
    allDone :: [EventsDone]
    allDone = do
        let (tss,em) = schedule
        tss' <- inits tss
        em' <- traverse inits em -- wow!
        return (tss', em')

    allChecks :: [Check]
    allChecks = allDone >>= prevs

    prevs :: EventsDone -> [Check]
    prevs (tss,m) =
        [ ((init tss, m), Left (last tss))
        | not (null tss)
        ] ++
        [ ((tss, IM.adjust init i m), Right (i, last done))
        | i <- IM.keys m
        , let done = m IM.! i
        , not (null done)
        ]

    memo :: M.Map EventsDone (Future Log)
    memo = M.fromList [ (eventsDone, recreate eventsDone) | eventsDone <- allDone ]

    recreate :: EventsDone -> Future Log
    recreate m = case prevs m of
        [] -> initFuture [] (IM.size (snd m))
        (c:_) -> checkActual c

    check :: Check -> Bool
    check c = checkActual c `eqFuture` checkExpected c

    checkExpected :: Check -> Future Log
    checkExpected (prev, Left t)          = memo M.! first (++[t]) prev
    checkExpected (prev, Right (p,(t,e))) = memo M.! second (IM.adjust (++[(t,e)]) p) prev

    checkActual :: Check -> Future Log
    checkActual (prev, Left t) =
            currentTimePasses step rate t $
            memo M.! prev
    checkActual (prev, Right (p,(t,e))) =
            addEvent step rate p t (handle <$> e) $
            memo M.! prev

    mkReport :: Check -> CheckReport
    mkReport c = (c, memo M.! fst c, checkExpected c, checkActual c)

-- The quickcheck test, with reporting

testPrediction :: Blind GenEventsTodo -> Property
testPrediction (Blind (GenEventsTodo schedule)) = do
   reportFailedCheck (head failed) `whenFail` null failed
  where failed = failedChecks schedule

reportFailedCheck :: CheckReport -> IO ()
reportFailedCheck (c, before, expected, actual) = do
    putStrLn "Failed Check"
    putStrLn "History:"
    putStr $ showHistory (fst c)
    putStrLn "Event:"
    print (snd c)
    putStrLn "Before:"
    printInternalState showLog before
    putStrLn "Expected:"
    printInternalState showLog expected
    putStrLn "Actual:"
    printInternalState showLog actual
    putStrLn ""

showHistory :: EventsDone -> String
showHistory (tss,m) = unlines $
    ("Queried at: " ++ intercalate " " (map show tss)) : map go (IM.toList m)
  where
    go (p, tes) = "Player " ++ show p ++ ": " ++ intercalate " " (map ste tes)
    ste (t, Nothing) = show t
    ste (t, Just e) = show e ++ "@" ++ show t

showLog :: Log -> String
showLog l = intercalate " " (map sle (reverse l))
  where
    sle (Left x)  = show x
    sle (Right x) = "["++show x++"]"

-- The main entry point.
-- Set the exit code to please Cabal.
main = do
    res <- quickCheckWithResult args testPrediction
    case res of
        Success {} -> exitSuccess
        _          -> exitFailure
  where
    args = stdArgs { maxSize = 30 } -- more gets too large

-- QuickCheck utilities

firstA :: Applicative f => (t -> f a1) -> (t, a) -> f (a1, a)
firstA f (a,b) = (,) <$> f a <*> pure b

-- An applicative functor that shrinks exactly one element
-- Also see http://stackoverflow.com/a/41944525/946226
data ShrinkOne a = ShrinkOne a [a]

instance Functor ShrinkOne where
    fmap f (ShrinkOne o s) = ShrinkOne (f o) (map f s)

instance Applicative ShrinkOne where
    pure x = ShrinkOne x []
    ShrinkOne f fs <*> ShrinkOne x xs = ShrinkOne (f x) (map ($x) fs ++ map f xs)

shrinkOne :: Arbitrary a => a -> ShrinkOne a
shrinkOne x = ShrinkOne x (shrink x)

unShrinkOne :: ShrinkOne t -> [t]
unShrinkOne (ShrinkOne _ xs) = xs

-- Remove one element of the list
listShrinkOne :: [a] -> ShrinkOne [a]
listShrinkOne xs = ShrinkOne xs (listShrink xs)
  where
    listShrink []     = []
    listShrink (x:xs) = [ xs ] ++ [ x:xs' | xs' <- listShrink xs ]

-- Remove one element of the list, or reduce a singleton with
-- the given function.
listShrinkOne' :: (a -> [a]) -> [a] -> ShrinkOne [a]
listShrinkOne' _ []  = ShrinkOne [] []
listShrinkOne' f [x] = ShrinkOne [x] (map (:[]) (f x))
listShrinkOne' _ xs  = listShrinkOne xs



