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

-- | Common code for the tests
module Common where

import Test.QuickCheck
import qualified Data.IntMap as IM

-- Generate a game history 
type Event = Char
type TimeStamp = Double
type TEvent = (TimeStamp, Maybe Event)
type EventSchedule = ([TimeStamp], IM.IntMap [TEvent])

genTimeStamps :: Gen [Double]
genTimeStamps = fmap (drop 1 . scanl (+) 0) $
    sized $ \n -> do
        k <- choose (0,n)
        sequence [ genOne | _ <- [1..k] ]
  where
    genOne = frequency [(5, getPositive <$> arbitrary), (1, return 0)]

genEventSchedule = do
        -- get ascending positive timestamps
        tss <- genTimeStamps
        qts <- sublistOf tss
        p1ts  <- sublistOf tss
        p2ts  <- sublistOf tss
        -- some are just pings, some are real events
        p1 <- traverse (makePingOrEvent 'A') p1ts
        p2 <- traverse (makePingOrEvent 'B') p2ts
        return $ (qts, IM.fromList [(0,p1), (1,p2)])
      where makePingOrEvent i ts = do
               coin <- arbitrary
               if coin then return $ (ts, Nothing)
                       else return $ (ts, Just i)

    -- shrinking removes entries of the list, and
    -- further reduces singleton lists mildly
    -- (the original shrink would get us too close to epsilon)
shrinkEventSchedule (tss,m) = unShrinkOne $
    ((,) <$> listShrinkOne' mildlySmaller tss
         <*> traverse (listShrinkOne' (firstA mildlySmaller)) m)
  where mildlySmaller x | x > 1     = [x - 1]
                        | otherwise = []



forAllSchedules :: Testable prop => (EventSchedule -> prop) -> Property
forAllSchedules = forAllShrink genEventSchedule shrinkEventSchedule

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



