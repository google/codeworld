{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

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

module Internal.Prelude (
    ifThenElse, -- For RebindableSyntax

    -- Comparison
    (==),
    (/=),

    -- Truth
    Truth,
    Bool(..),
    (&&),
    (||),
    not,
    otherwise,

    -- Currying and uncurrying
    toOperator,
    fromOperator,

    -- Basic functions
    P.id,
    (.),

    -- Tuples
    firstOfPair,
    secondOfPair,

    -- Failures
    error, -- Text version
    P.undefined,

    -- List functions
    (P.++),
    empty,
    contains,
    length,
    at,
    (#),
    any,
    all,
    none,
    repeated,
    repeating,
    first,
    last,
    rest,
    while,
    until,
    after,
    concatenation,
    L.subsequences,
    L.permutations,
    sorted,
    reversed,
    unique,
    transposed,
    combined,

    -- Maybe
    P.Maybe(..),
    withDefault,
    hasValue,
    definitely,

    -- Random numbers
    fromRandomSeed,
    shuffled
    ) where

import qualified "base" Prelude as P
import qualified "base" Data.Maybe as P
import "base" Prelude (Bool, (.), ($))

import Data.Bits (xor)
import Data.Function (on)
import qualified Data.List as L

import Internal.Num
import Internal.Text
import Internal.Truth

import System.Random hiding (split)
import System.Random.Shuffle (shuffle')

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

-- | Returns the first element of an ordered pair.
firstOfPair :: (a, b) -> a
firstOfPair (a, b) = a

-- | Returns the second element of an ordered pair.
secondOfPair :: (a, b) -> b
secondOfPair (a, b) = b

-- | Fails with an error message.
error :: Text -> a
error = P.error . toString

-- | Determines whether a list is empty or not.
empty :: [a] -> Truth
empty [] = P.True
empty _ = P.False

-- | Determines whether a value is a member of a list or not.
contains :: ([a], a) -> Truth
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
(#) :: [a] -> Number -> a
(#) = toOperator(at)
infixl 9 #

-- | Determines if any proposition in a list is true.
--
-- For example, `any([even(n) | n <- [1,2,3]])` is `True`, because 2 is even.
any :: [Truth] -> Truth
any = P.or

-- | Determines if all propositions in a list are true.
--
-- For example, `all([even(n) | n <- [2,3,4]])` is `False`, because 3 is not even.
all :: [Truth] -> Truth
all = P.and

-- | Determines if all propositions in a list are false.
--
-- For example, `none([odd(n) | n <- [2,3,4]])` is `False`, because 3 is odd.
none :: [Truth] -> Truth
none = P.not . any

-- | Forms a list by repeating a source list some number of times.
repeated :: ([a], Number) -> [a]
repeated (xs, 0) = []
repeated (xs, n) = xs P.++ repeated(xs, n-1)

-- | Forms a list by repeating a source list forever.
repeating :: [a] -> [a]
repeating = P.cycle

-- | Gives the first members of a list, up to the given number.
first :: ([a], Number) -> [a]
first (xs, n) = P.take (toInt n) xs

-- | Gives the last members of a list, up to the given number.
last :: ([a], Number) -> [a]
last (xs, n) = P.drop (P.length xs P.- toInt n) xs

-- | Gives all members of a list after the given number.
--
-- In general, `xs = first(xs, n) ++ rest(xs, n)`.
rest :: ([a], Number) -> [a]
rest (xs, n) = P.drop (toInt n) xs

-- | Gives the longest prefix of a list for which a condition is true.
--
-- For example, `while([2,4,5,6], even) = [2,4]`.
while :: ([a], a -> Truth) -> [a]
while (xs, p) = P.takeWhile p xs

-- | Gives the longest prefix of a list for which a condition is false.
--
-- For example, `until([2,4,5,6], odd) = [2,4]`.
until :: ([a], a -> Truth) -> [a]
until (xs, p) = P.takeWhile (P.not . p) xs

-- | Gives the remaining portion of a list after the longest prefix
-- for which a condition is true.
--
-- In general, `xs = while(xs, cond) ++ after(xs, cond)
after :: ([a], a -> Truth) -> [a]
after (xs, p) = P.dropWhile p xs

-- | Gives the concatenation of all of the lists in its input.
concatenation :: [[a]] -> [a]
concatenation = P.concat

-- | Gives a list of numbers reordered into increasing order.
sorted :: [Number] -> [Number]
sorted = L.sort

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

-- For some reason, randoms numbers seem to give conspicuously similar
-- results early in the sequence, so we throw away a few to get better
-- mixing.
numToStdGen :: Number -> StdGen
numToStdGen r = mkStdGen (a `xor` P.fromIntegral b `xor` P.fromIntegral c)
  where (sig, a) = P.decodeFloat (P.realToFrac r)
        (b,   c) = sig `P.divMod` (2 P.^ 31)

randomsFrom :: StdGen -> [Number]
randomsFrom g = fromDouble a : randomsFrom g2
  where (a, g2) = random g

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
hasValue :: P.Maybe a -> Truth
hasValue P.Nothing = P.False
hasValue (P.Just _) = P.True

-- | Extracts the value from a Maybe, and crashes the program if there
-- is no such value.
definitely :: P.Maybe a -> a
definitely (P.Just a) = a
definitely P.Nothing = P.error "Used definitely on a value of Nothing."

fromRandomSeed :: Number -> [Number]
fromRandomSeed = randomsFrom . numToStdGen
