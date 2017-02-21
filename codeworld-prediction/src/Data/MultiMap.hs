-- |
-- Module       : Data.MultiMap
-- Copyright    : (c) Joachim Breitner 2017
-- License      : Apache-2.0
--
-- Maintainer   : mail@joachim-breitner.de
--
-- A simple MultiMap.
--
-- This differs from the one in the @multimap@ package by using
-- 'Data.Sequence.Seq' for efficient insert-at-end and other improved speeds.
--
-- Also only supports the operations required by CodeWorld for now.

{-# LANGUAGE TupleSections #-}
module Data.MultiMap where

import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Foldable
import Data.Bifunctor

newtype MultiMap k v = MM (M.Map k (S.Seq v)) deriving (Show, Eq)

empty :: MultiMap k v
empty = MM M.empty

null :: MultiMap k v -> Bool
null (MM m) = M.null m

insertL :: Ord k => k -> v -> MultiMap k v -> MultiMap k v
insertL k v (MM m) = MM (M.alter (Just . maybe (S.singleton v) (v S.<|)) k m)

insertR :: Ord k => k -> v -> MultiMap k v -> MultiMap k v
insertR k v (MM m) = MM (M.alter (Just . maybe (S.singleton v) (S.|> v)) k m)

toList :: MultiMap k v -> [(k,v)]
toList (MM m) = concatMap (\(k,vs) -> map (k,) (Data.Foldable.toList vs)) (M.toList m)

-- TODO: replace with M.spanAntitone once containers is updated
mapSpanAntitone p = bimap M.fromDistinctAscList M.fromDistinctAscList . span (p.fst) . M.toList

spanAntitone :: (k -> Bool) -> MultiMap k v -> (MultiMap k v, MultiMap k v)
spanAntitone p (MM m) = bimap MM MM (mapSpanAntitone p m)

union :: Ord k => MultiMap k v -> MultiMap k v -> MultiMap k v
union (MM m1) (MM m2) = MM (M.unionWith (S.><) m1 m2)

keys :: MultiMap k v -> [k]
keys (MM m) = M.keys m
