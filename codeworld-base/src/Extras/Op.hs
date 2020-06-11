{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | An experimental module with a collection of composition and
-- application operators in different flavors.
module Extras.Op where

import Prelude
import "base" Prelude (filter, fst, map, snd)

-- | Applicative operator, used to apply a function to all the elements of a
-- list
(<$>) :: (a -> b) -> [a] -> [b]
f <$> l = map f l

infixr 1 <$>

-- | Alternative syntax for the applicative operator, which indicates
-- direction of data flow (right to left)
(<$) :: (a -> b) -> [a] -> [b]
(<$) = (<$>)

infixr 1 <$

-- | Alternative syntax for the applicative operator, which indicates
-- direction of data flow (left to right)
($>) :: [a] -> (a -> b) -> [b]
l $> f = f <$ l

infixl 1 $>

-- | Selective operator, used to keep only those elements in the
-- given list that satisfy the given condition
(<?>) :: (a -> Truth) -> [a] -> [a]
cond <?> l = filter cond l

infixr 1 <?>

-- | A predicate that is True whenever one of its arguments is True
(<|>) :: (a -> Truth) -> (a -> Truth) -> (a -> Truth)
(p1 <|> p2) (x) = p1 (x) || p2 (x)

infixl 2 <|>

-- | A predicate that is True whenever both of its arguments are True
(<&>) :: (a -> Truth) -> (a -> Truth) -> (a -> Truth)
(p1 <&> p2) (x) = p1 (x) && p2 (x)

infixl 3 <&>

-- | Reverses the order of function application, so that the argument is
-- first and the function is second
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 1 |>

-- | Makes function application explicit, so @f <| x@ is the same as @f(x)@
(<|) :: (a -> b) -> a -> b
f <| x = f (x)

infixr 1 <|

-- | Function composition, where data flows from left to right
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(f .> g) (x) = g (f (x))

infixl 1 .>

-- | Function composition, where data flows from right to left
(.<) :: (b -> c) -> (a -> b) -> (a -> c)
(f .< g) (x) = f (g (x))

infixl 1 .<

-- | Unicode version of the function application operator (flowing
-- right to left)
(⇐) = (<|)

infixr 1 ⇐

-- | Unicode version of the function application operator (flowing
-- left to right)
(⇒) = (|>)

infixl 1 ⇒

-- | Simulates object notation by allowing you to write @x.#method@ instead
-- of @method(x)@
(.#) :: a -> (a -> b) -> b
x .# f = f (x)

infixl 9 .#

-- | Object notation for the applicative operator
(.$) :: [a] -> (a -> b) -> [b]
l .$ f = map f l

infixl 9 .$

-- | Object notation for the selective operator
(.?) :: [a] -> (a -> Truth) -> [a]
l .? cond = filter cond l

infixl 9 .?
