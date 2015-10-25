{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

{-
  Copyright 2015 Google Inc. All rights reserved.

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

module Internal.Prelude (
    ifThenElse, -- For RebindableSyntax

    -- Comparison
    (==),
    (/=),

    -- Bool
    Bool(..),
    (P.&&),
    (P.||),
    P.not,
    P.otherwise,

    -- Currying and uncurrying
    toOperator,
    fromOperator,

    -- Basic functions
    P.id,
    P.const,
    (.),

    -- Failures
    error, -- Text version
    P.undefined,

    -- List functions
    (P.++),
    first,
    P.last,
    rest,
    empty,
    contains,
    length,
    at,
    (!!),
    any,
    all,
    repeated,
    repeating,
    cycle,

    concatenation,
    L.subsequences,
    L.permutations,
    sorted,
    sort,
    reversed,
    unique,
    transposed,
    combined,

    take,
    drop,
    split,
    takeWhile,
    dropWhile,
    splitWhen,

    -- Maybe
    P.Maybe(..),
    withDefault,
    hasValue,
    definitely,

    -- Random numbers
    seedRandoms,
    fromRandomSeed,
    shuffled,
    shuffle
    ) where

import qualified "base" Prelude as P
import qualified "base" Data.Maybe as P
import "base" Prelude (Bool, (.))

import Data.Function (on)
import qualified Data.List as L

import Data.Text (Text)
import qualified Data.Text as T

import Internal.DeepEq
import Internal.Num
import Internal.Text

import System.Random hiding (split)
import System.Random.Shuffle (shuffle')

ifThenElse :: Bool -> a -> a -> a
ifThenElse a b c = if a then b else c

infix 4 ==, /=

-- | Compares values to see if they are equal.
(==) :: a -> a -> Bool
a == b = deepEq a b

-- | Compares values to see if they are not equal.
-- Note that `a /= b` is the same as `not (a == b)`.
(/=) :: a -> a -> Bool
a /= b = P.not (a == b)

-- | Converts a function to an operator.
--
-- Example use:
--
--    f(x,y) = 2*x + y
--    (%) = toOperator(f)
--
--    eight = 3 % 2
--
-- This has the same effect as defining % as:
--
--    x % y = 2*x + y
--    eight = 3 % 2
toOperator :: ((a, b) -> c) -> (a -> b -> c)
toOperator = P.curry

-- | Converts an operator into a normal function.
--
-- Example use:
--
--    divide = fromOperator(/)
--    four = divide(16, 4)
fromOperator :: (a -> b -> c) -> ((a, b) -> c)
fromOperator = P.uncurry

-- | Fails with an error message.
error :: Text -> a
error = P.error . T.unpack

-- | Gives the first member of a list.
first :: [a] -> a
first = P.head

-- | Gives all members of a list after the first one.
rest :: [a] -> [a]
rest = P.tail

-- | Determines whether a list is empty or not.
empty :: [a] -> Bool
empty [] = P.True
empty _ = P.False

-- | Determines whether a value is a member of a list or not.
contains :: ([a], a) -> Bool
contains (xs, x) = P.any (== x) xs

-- | Gives the length of a list.
length :: [a] -> Number
length = fromInt . P.length

-- | Gives the member of a list at a given index.
-- Indices start at 0.
at :: ([a], Number) -> a
at (xs, n) = xs P.!! toInt n

-- | Gives the member of a list at a given index.
-- Indices start at 0.
(!!) :: [a] -> Number -> a
(!!) = toOperator(at)
infixl 9 !!

-- | Determines if any proposition in a list is true.
--
-- For example, `any([even(n) | n <- [1,2,3]])` is `True`, because 2 is even.
any :: [Bool] -> Bool
any = P.or

-- | Determines if all propositions in a list are true.
--
-- For example, `all([even(n) | n <- [2,3,4]])` is `False`, because 3 is not even.
all :: [Bool] -> Bool
all = P.and

-- | Forms a list by repeating a source list some number of times.
repeated :: ([a], Number) -> [a]
repeated (xs, 0) = []
repeated (xs, n) = xs P.++ repeated(xs, n-1)

-- | Forms a list by repeating a source list forever.
repeating :: [a] -> [a]
repeating = P.cycle

-- | Forms a list by repeating a source list forever.
cycle :: [a] -> [a]
cycle = P.cycle
{-# WARNING cycle "Please use repeating(...) instead of cycle(...)" #-}

-- | Gives the concatenation of all of the lists in its input.
concatenation :: [[a]] -> [a]
concatenation = P.concat

-- | Gives a list of numbers reordered into increasing order.
sorted :: [Number] -> [Number]
sorted = L.sort

-- | Gives a list of numbers reordered into increasing order.
sort :: [Number] -> [Number]
sort = L.sort
{-# WARNING sort "Please use sorted(...) instead of sort(...)" #-}

-- | Gives a list in the opposite order of the original.
reversed :: [a] -> [a]
reversed = P.reverse

-- | Gives a list with all duplicate members removed.
unique :: [a] -> [a]
unique = L.nubBy deepEq

transposed :: [[a]] -> [[a]]
transposed = L.transpose

-- | Combines a list of values into a single value, by merging
-- members with a function.  The function should take two parameters,
-- and should be associative (so `f(x,f(y,z)) = f(f(x,y),z)`).  The
-- list should be non-empty.
--
-- For example, `combined(fromOperator(+), [1, 3, 5])` is equal to `9`.
combined :: ((a, a) -> a, [a]) -> a
combined (f, [])   = P.error "combined was applied to an empty list."
combined (f, [x])  = x
combined (f, x:xs) = f(x, combined(f, xs))

take :: ([a], Number) -> [a]
take (xs, n) = P.take (toInt n) xs

drop :: ([a], Number) -> [a]
drop (xs, n) = P.drop (toInt n) xs

split :: ([a], Number) -> ([a], [a])
split (xs, n) = P.splitAt (toInt n) xs

takeWhile :: ([a], a -> Bool) -> [a]
takeWhile (xs, f) = P.takeWhile f xs

dropWhile :: ([a], a -> Bool) -> [a]
dropWhile (xs, f) = P.dropWhile f xs

splitWhen :: ([a], a -> Bool) -> ([a], [a])
splitWhen (xs, f) = P.break f xs

numToStdGen :: Number -> StdGen
numToStdGen r = mkStdGen (P.round (P.realToFrac r P.* P.fromIntegral (P.maxBound :: P.Int)))

randomsFrom :: StdGen -> [Number]
randomsFrom g = fromDouble a : randomsFrom g2
  where (a, g2) = random g

shuffle :: ([a], Number) -> [a]
shuffle = shuffled

shuffled :: ([a], Number) -> [a]
shuffled ([], r) = []
shuffled (xs, r) = shuffle' xs (P.length xs) (numToStdGen r)

-- | Converts a Maybe value to a plain value, by using a default.
--
-- For example, `withDefault(Nothing, 5)` is equal to 5, while
-- `withDefault(Just(3), 5)` is equal to 3.
withDefault :: (P.Maybe a, a) -> a
withDefault (m, d) = P.fromMaybe d m

-- | Determines if a Maybe has a value.
hasValue :: P.Maybe a -> Bool
hasValue P.Nothing = P.False
hasValue (P.Just _) = P.True

-- | Extracts the value from a Maybe, and crashes the program if there
-- is no such value.
definitely :: P.Maybe a -> a
definitely (P.Just a) = a
definitely P.Nothing = P.error "Used definitely on a value of Nothing."

fromRandomSeed :: Number -> [Number]
fromRandomSeed = randomsFrom . numToStdGen

seedRandoms :: Number -> [Number]
seedRandoms = fromRandomSeed

{-# WARNING shuffle "Please use shuffled(...) instead of shuffle(...)" #-}
{-# WARNING seedRandoms "Please use fromRandomSeed(...) instead of seedRandoms(...)" #-}
