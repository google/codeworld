{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module Internal.Prelude (
    -- Miscellaneous types
    Bool(..),
    (P.&&),
    (P.||),
    P.not,
    P.otherwise,
    P.Maybe(..),
    P.maybe,
    P.Either(..),
    P.either,

    -- Comparison
    P.Eq(..),

    -- Tuples
    P.fst,
    P.snd,
    P.curry,
    P.uncurry,

    -- Miscellaneous
    P.id,
    P.const,
    (.),
    P.flip,
    (P.$),
    P.until,
    P.asTypeOf,
    error, -- Text version
    P.undefined,
    P.seq,
    (P.$!),
    ifThenElse, -- For RebindableSyntax

    -- List functions
    P.map,
    (P.++),
    P.filter,
    P.head,
    P.tail,
    P.init,
    P.last,
    P.null,
    length, -- specialized to Number
    (!!), -- specialized to Number
    P.reverse,
    P.foldl,
    P.foldl1,
    P.foldr,
    P.foldr1,
    P.and,
    P.or,
    P.any,
    P.all,
    P.concat,
    P.concatMap,
    P.scanl,
    P.scanl1,
    P.scanr,
    P.scanr1,
    P.iterate,
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
    P.elem,
    P.notElem,
    P.lookup,
    P.zip,
    P.zip3,
    P.zipWith,
    P.zipWith3,
    P.unzip,
    P.unzip3,

    ) where

import qualified "base" Prelude as P
import "base" Prelude (Bool, (.))

import Data.Text (Text)
import qualified Data.Text as T

import Internal.Num
import Internal.Text

error :: Text -> a
error = P.error . T.unpack

ifThenElse :: Bool -> a -> a -> a
ifThenElse a b c = if a then b else c

length :: [a] -> Number
length = fromInt . P.length

(!!) :: [a] -> Number -> a
xs !! n = xs P.!! toInt n

replicate :: Number -> a -> [a]
replicate n x = P.replicate (toInt n) x

take :: Number -> [a] -> [a]
take n xs = P.take (toInt n) xs

drop :: Number -> [a] -> [a]
drop n xs = P.drop (toInt n) xs

splitAt :: Number -> [a] -> ([a], [a])
splitAt n xs = P.splitAt (toInt n) xs
