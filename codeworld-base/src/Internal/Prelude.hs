{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

{-
  Copyright 2018 The CodeWorld Authors. All rights reserved.

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
module Internal.Prelude
    ( ifThenElse -- For RebindableSyntax
    , fail -- for RebindableSyntax
    -- Comparison
    , (==)
    , (/=)
    -- Truth
    , Truth
    , Bool(..)
    , (&&)
    , (||)
    , not
    , otherwise
    -- Currying and uncurrying
    , toOperator
    , fromOperator
    -- Basic functions
    , id
    , (.)
    -- Tuples
    , firstOfPair
    , secondOfPair
    -- Failures
    , error -- Text version
    , undefined
    -- List functions
    , (P.++)
    , empty
    , contains
    , length
    , at
    , ( # )
    , any
    , all
    , none
    , repeated
    , repeating
    , first
    , last
    , rest
    , groups
    , while
    , until
    , after
    , concatenation
    , L.subsequences
    , L.permutations
    , sorted
    , reversed
    , unique
    , transposed
    , combined
    -- Maybe
    , Maybe(..)
    , withDefault
    , hasValue
    , definitely
    -- Random numbers
    , fromRandomSeed
    , randomsFrom
    , randomNumbers
    , shuffled
    ) where

import qualified "base" Data.Maybe as P
import qualified "base" Prelude as P
import "base" Prelude (Bool, ($))

import Data.Bits (xor)
import Data.Function (on)
import qualified Data.List as L

import Internal.Num
import Internal.Text
import Internal.Truth

import System.Random hiding (split)
import System.Random.Shuffle (shuffle')

import GHC.Stack (HasCallStack, withFrozenCallStack)

id :: a -> a
id x = x

{-# WARNING
id "Please define your own identity function."
 #-}

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = (P..)

{-# WARNING
(.) "Please implement function composition with arguments."
 #-}

-- | Converts a function to an operator.
--
-- Example use:
--
-- >  f(x,y) = 2*x + y
-- >  (%) = toOperator(f)
-- >
-- >  eight = 3 % 2
--
-- This has the same effect as defining % as:
--
-- >  x % y = 2*x + y
-- >  eight = 3 % 2
toOperator :: ((a, b) -> c) -> (a -> b -> c)
toOperator = P.curry

-- | Converts an operator into a normal function.
--
-- Example use:
--
-- >  divide = fromOperator(/)
-- >  four = divide(16, 4)
fromOperator :: (a -> b -> c) -> ((a, b) -> c)
fromOperator = P.uncurry

-- | Returns the first element of an ordered pair.
firstOfPair :: (a, b) -> a
firstOfPair (a, b) = a

-- | Returns the second element of an ordered pair.
secondOfPair :: (a, b) -> b
secondOfPair (a, b) = b

-- | Fails with an error message.
error :: HasCallStack => Text -> a
error msg = withFrozenCallStack (P.error (toString msg))

-- | Represents an undefined value.  This lets you compile programs with unfinished
-- values.  If the value is needed, the program will crash.
undefined :: HasCallStack => a
undefined = withFrozenCallStack (P.error "Value is not defined.")

-- | Fails with an error message.  This is required (though apparently unused)
-- by the desugaring for pattern binds in list comprehensions.
fail :: HasCallStack => P.String -> a
fail msg = withFrozenCallStack (P.error msg)

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

-- | Gives a list with a replaced element at an index.
-- Indices start at 1.
at :: HasCallStack => ([a], a, Number) -> [a]
at (list, val, idx)
    | not (isInteger (idx)) = withFrozenCallStack $ idxErrorNonInt idx
    | idx <= 0 = withFrozenCallStack $ idxErrorNonPos idx
    | otherwise = withFrozenCallStack $ go idx list
  where
    go _ [] = idxErrorTooLarge idx
    go 1 (x:xs) = val : xs
    go n (x:xs) = x : go (n - 1) xs

-- | Gives the member of a list at a given index.
-- Indices start at 1.
(#) :: HasCallStack => [a] -> Number -> a
list # idx
    | not (isInteger (idx)) = withFrozenCallStack $ idxErrorNonInt idx
    | idx <= 0 = withFrozenCallStack $ idxErrorNonPos idx
    | otherwise = withFrozenCallStack $ go idx list
  where
    go _ [] = idxErrorTooLarge idx
    go 1 (x:xs) = x
    go n (x:xs) = go (n - 1) xs

infixl 9 #

idxErrorNonInt :: HasCallStack => Number -> a
idxErrorNonInt idx = P.error "Non-integer list index is not allowed."

idxErrorNonPos :: HasCallStack => Number -> a
idxErrorNonPos idx =
    P.error "List index must be positive. Numbering starts at 1."

idxErrorTooLarge :: HasCallStack => Number -> a
idxErrorTooLarge idx = P.error "List index is too large."

-- | Determines if any proposition in a list is true.
--
-- For example, @any([even(n) | n <- [1,2,3]])@ is 'True', because 2 is even.
any :: [Truth] -> Truth
any = P.or

-- | Determines if all propositions in a list are true.
--
-- For example, @all([even(n) | n <- [2,3,4]])@ is 'False', because 3 is not even.
all :: [Truth] -> Truth
all = P.and

-- | Determines if all propositions in a list are false.
--
-- For example, @none([odd(n) | n <- [2,3,4]])@ is 'False', because 3 is odd.
none :: [Truth] -> Truth
none = P.not . any

-- | Forms a list by repeating a source list some number of times.
repeated :: ([a], Number) -> [a]
repeated (xs, 0) = []
repeated (xs, n) = xs P.++ repeated (xs, n - 1)

-- | Forms a list by repeating a source list forever.
repeating :: [a] -> [a]
repeating = P.cycle

-- | Gives the first members of a list, up to the given number.
first :: HasCallStack => ([a], Number) -> [a]
first (xs, n) = withFrozenCallStack (P.take (toInt n) xs)

-- | Gives the last members of a list, up to the given number.
last :: HasCallStack => ([a], Number) -> [a]
last (xs, n) = withFrozenCallStack (P.drop (P.length xs P.- toInt n) xs)

-- | Gives all members of a list after the given number.
--
-- In general, @xs = first(xs, n) ++ rest(xs, n)@.
rest :: HasCallStack => ([a], Number) -> [a]
rest (xs, n) = withFrozenCallStack (P.drop (toInt n) xs)

-- | Converts a list of elements into a list of smaller lists, each of the
-- given length.
--
-- For example, @[ (x, y) | [x, y] <- groups(randomNumbers(42), 2) ]@.
groups :: ([a], Number) -> [[a]]
groups ([], n) = []
groups (xs, n) = withFrozenCallStack $
    P.take (toInt n) xs : groups (P.drop (toInt n) xs, n)

-- | Gives the longest prefix of a list for which a condition is true.
--
-- For example, @while([2,4,5,6], even) = [2,4]@.
while :: ([a], a -> Truth) -> [a]
while (xs, p) = P.takeWhile p xs

-- | Gives the longest prefix of a list for which a condition is false.
--
-- For example, @until([2,4,5,6], odd) = [2,4]@.
until :: ([a], a -> Truth) -> [a]
until (xs, p) = P.takeWhile (P.not . p) xs

-- | Gives the remaining portion of a list after the longest prefix
-- for which a condition is true.
--
-- In general, @xs = while(xs, cond) ++ after(xs, cond)@.
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
-- and should be associative (so @f(x,f(y,z)) = f(f(x,y),z)@).  The
-- list should be non-empty.
--
-- For example, @combined(fromOperator(+), [1, 3, 5])@ is equal to 9.
combined :: HasCallStack => ((a, a) -> a, [a]) -> a
combined (f, []) = withFrozenCallStack (P.error "Empty list is not allowed.")
combined (f, [x]) = x
combined (f, x:xs) = f (x, combined (f, xs))

{-# WARNING
combined "Pleased use recursion instead of combined(...)."
 #-}

-- For some reason, randoms numbers seem to give conspicuously similar
-- results early in the sequence, so we throw away a few to get better
-- mixing.
numToStdGen :: Number -> StdGen
numToStdGen r = mkStdGen (a `xor` P.fromIntegral b `xor` P.fromIntegral c)
  where
    (sig, a) = P.decodeFloat (P.realToFrac r)
    (b, c) = sig `P.divMod` (2 P.^ 31)

randomsFrom :: StdGen -> [Number]
randomsFrom g = fromDouble a : randomsFrom g2
  where
    (a, g2) = random g

shuffled :: ([a], Number) -> [a]
shuffled ([], r) = []
shuffled (xs, r) = shuffle' xs (P.length xs) (numToStdGen r)

data Maybe a
    = Nothing
    | Just a

{-# WARNING
Maybe "Please use your own data type instead of Maybe and friends."
 #-}

{-# WARNING
Just "Please use your own data type instead of Maybe and friends."
 #-}

{-# WARNING
Nothing "Please use your own data type instead of Maybe and friends."
 #-}

-- | Converts a Maybe value to a plain value, by using a default.
--
-- For example, @withDefault(Nothing, 5)@ is equal to 5, while
-- @withDefault(Just(3), 5)@ is equal to 3.
withDefault :: (Maybe a, a) -> a
withDefault (Nothing, d) = d
withDefault (Just a, _) = a

{-# WARNING
withDefault "Please use your own data type instead of Maybe and friends."
 #-}

-- | Determines if a Maybe has a value.
hasValue :: Maybe a -> Truth
hasValue Nothing = P.False
hasValue (Just _) = P.True

{-# WARNING
hasValue "Please use your own data type instead of Maybe and friends."
 #-}

-- | Extracts the value from a Maybe, and crashes the program if there
-- is no such value.
definitely :: HasCallStack => Maybe a -> a
definitely (Just a) = a
definitely Nothing =
    withFrozenCallStack (P.error "Expected a value; found Nothing.")

{-# WARNING
definitely "Please use your own data type instead of Maybe and friends."
 #-}

fromRandomSeed :: Number -> [Number]
fromRandomSeed = randomNumbers

{-# WARNING
fromRandomSeed "Please use randomNumbers instead of fromRandomSeed."
 #-}

randomNumbers :: Number -> [Number]
randomNumbers = randomsFrom . numToStdGen
