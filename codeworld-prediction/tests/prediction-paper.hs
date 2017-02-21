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

type TimeStamp = Double
type TEvent = (TimeStamp, Maybe Event)

type EventsTodo = (IM.IntMap [TEvent])
type EventsDone = (IM.IntMap [TEvent])

type LogEntry = Either Double Event
type Book = [LogEntry]

type Check = (EventsDone, (Int, TEvent))
type CheckReport = (Check, Log Book, Log Book, Log Book)

-- Fake state and handle functions

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


-- Generation of random schedules

newtype GenEventsTodo = GenEventsTodo EventsTodo deriving Show

genTimeStamps :: Gen [Double]
genTimeStamps = fmap (drop 1 . scanl (+) 0) $
    sized $ \n -> do
        k <- choose (0,n)
        sequence [ genOne | _ <- [1..k] ]
  where
    genOne = frequency [(5, getPositive <$> arbitrary), (1, return 0)]

instance Arbitrary GenEventsTodo where
    arbitrary = do
        -- get ascending positive timestamps
        tss <- genTimeStamps
        qts <- sublistOf tss
        p1ts  <- sublistOf tss
        p2ts  <- sublistOf tss
        -- some are just pings, some are real events
        p1 <- traverse (makePingOrEvent 'A') p1ts
        p2 <- traverse (makePingOrEvent 'B') p2ts
        return $ GenEventsTodo (IM.fromList [(0,p1), (1,p2)])
      where makePingOrEvent i ts = do
               coin <- arbitrary
               if coin then return $ (ts, Nothing)
                       else return $ (ts, Just i)

    -- shrinking removes poitns of the list, and
    -- further reduces singleton lists mildly
    -- (the original shrink would get us too close to epsilon)
    shrink (GenEventsTodo m) =
        map GenEventsTodo $
        unShrinkOne $ traverse (listShrinkOne' (firstA mildlySmaller)) m
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
    listShrink (y:ys) = [ ys ] ++ [ y:ys' | ys' <- listShrink ys ]

-- Remove one element of the list, or reduce a singleton with
-- the given function.
listShrinkOne' :: (a -> [a]) -> [a] -> ShrinkOne [a]
listShrinkOne' _ []  = ShrinkOne [] []
listShrinkOne' f [x] = ShrinkOne [x] (map (:[]) (f x))
listShrinkOne' _ xs  = listShrinkOne xs



