{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

{-
  Copyright 2014 Google Inc. All rights reserved.

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
    -- Miscellaneous types
    Bool(..),
    (P.&&),
    (P.||),
    P.not,
    P.otherwise,
    P.Maybe(..),
    withDefault,
    maybe,
    P.Either(..),
    either,

    -- Comparison
    (==),
    (/=),

    -- Tuples
    P.fst,
    P.snd,
    toOperator,
    fromOperator,

    -- Miscellaneous
    P.id,
    P.const,
    (.),
    flip,
    until,
    error, -- Text version
    P.undefined,
    ifThenElse, -- For RebindableSyntax

    -- List functions
    map,
    (P.++),
    filter,
    first,
    rest,
    P.init,
    P.last,
    P.null,
    length, -- specialized to Number
    (!!), -- specialized to Number
    P.reverse,
    P.and,
    P.or,
    any,
    all,
    P.concat,
    concatMap,
    P.repeat,
    replicate, -- specialized to Number
    P.cycle,
    take, -- specialized to Number
    drop, -- specialized to Number
    splitAt, -- specialized to Number
    P.takeWhile,
    P.dropWhile,
    P.span,
    P.break,
    isMember,
    lookup,
    L.transpose,
    L.subsequences,
    L.permutations,
    nub, -- using deepEq
    sort, -- specialized to Number
    ) where

import qualified "base" Prelude as P
import qualified "base" Data.Maybe as P
import "base" Prelude (Bool, (.))

import qualified Data.List as L

import Data.Text (Text)
import qualified Data.Text as T

import Internal.DeepEq
import Internal.Num
import Internal.Text

-- | Converts a Maybe value to a plain value, by using a default.
--
-- For example, `withDefault(Nothing, 5)` is equal to 5, while
-- `withDefault(Just(3), 5)` is equal to 3.
withDefault :: (P.Maybe a, a) -> a
withDefault (m, d) = P.fromMaybe d m

-- | Gets a value from a Maybe value by applying either a default value or another
-- function.
--
-- For example, `maybe(Nothing, blank, circle)` is a picture, while
-- `maybe(Just(10), blank, circle)` is a picture of a circle with radius 10.
maybe :: (P.Maybe a, a -> b, b) -> b
maybe (m, f, b) = P.maybe b f m

-- | Gets a value from an `Either` value by applying either of two functions,
-- depending on if the value is the left or right possibility.
--
-- For example, `either(Left(5), circle, text)` is a circle with radius 5.  But
-- `either(Right("hello"), circle, text)` is a picture with the text `"hello"`.
either :: (P.Either a b, a -> c, b -> c) -> c
either (x, f, g) = P.either f g x

(==) :: a -> a -> Bool
a == b = deepEq a b

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

-- | Converts a function into a version that takes the arguments
-- in the opposite order.
--
-- Example:
--
--    f(x,y) = 2*x + y
--    g = flip(f)
--    eight = g(2,3)
flip :: ((a,b) -> c) -> ((b,a) -> c)
flip f (y,x) = f (x,y)

-- | Continues passing a value through a function until it meets a
-- condition.
--
-- Example:
--
--    seven = until(odd, (/ 2), 56)
until :: (a -> Bool, a -> a, a) -> a
until (p, f, x) = P.until p f x

error :: Text -> a
error = P.error . T.unpack

ifThenElse :: Bool -> a -> a -> a
ifThenElse a b c = if a then b else c

length :: [a] -> Number
length = fromInt . P.length

-- | Applies a function to each element of a list, and produces a list
-- of results.
--
-- For example, `map([1, 2, 3, 4, 5], circle)` is a list of circles of
-- different sizes.
map :: ([a], a -> b) -> [b]
map (xs, f) = P.map f xs

-- | Keeps only the elements of a list for which a function evaluates
-- to `True`.
--
-- For example, `filter([1, 2, 3, 4, 5], even)` is equal to `[2, 4]`.
filter :: ([a], a -> Bool) -> [a]
filter (xs, f) = P.filter f xs

first :: [a] -> a
first = P.head

rest :: [a] -> [a]
rest = P.tail

(!!) :: [a] -> Number -> a
xs !! n = xs P.!! toInt n

-- | Determines if any member of a list matches a condition.
--
-- For example, `any([1, 2, 3], even)` is `True`, because 2 is even.
any :: ([a], a -> Bool) -> Bool
any (xs, p) = P.any p xs

-- | Determines if all members of a list match a condition.
--
-- For example, `all([2, 3, 4], even)` is `False`, because 3 is not even.
all :: ([a], a -> Bool) -> Bool
all (xs, p) = P.all p xs

-- | Builds a list from all of the members in the lists produced by applying
-- a function to each element of the given list.
concatMap :: ([a], a -> [b]) -> [b]
concatMap (xs, f) = P.concatMap f xs

replicate :: (a, Number) -> [a]
replicate (x, n) = P.replicate (toInt n) x

take :: ([a], Number) -> [a]
take (xs, n) = P.take (toInt n) xs

drop :: ([a], Number) -> [a]
drop (xs, n) = P.drop (toInt n) xs

splitAt :: ([a], Number) -> ([a], [a])
splitAt (xs, n) = P.splitAt (toInt n) xs

takeWhile :: ([a], a -> Bool) -> [a]
takeWhile (xs, f) = P.takeWhile f xs

dropWhile :: ([a], a -> Bool) -> [a]
dropWhile (xs, f) = P.dropWhile f xs

isMember :: ([a], a) -> Bool
isMember (xs, x) = any (xs, (== x))

lookup :: ([(a, b)], a) -> P.Maybe b
lookup ([],              x) = P.Nothing
lookup ((k,v) : entries, x)
    | k == x      =  P.Just v
    | P.otherwise =  lookup (entries, x)

nub :: [a] -> [a]
nub = L.nubBy deepEq

sort :: [Number] -> [Number]
sort = L.sort

shuffle :: [a] -> [Number] -> ([a], [Number])
shuffle xs ns = (P.foldr f [] (sort ns'), unused) where
  (ns', unused) = P.splitAt (P.length xs) ns
  f r acc = (P.fromMaybe bad (P.lookup r (P.zip ns' xs))) : acc
  bad = P.error "impossible"
