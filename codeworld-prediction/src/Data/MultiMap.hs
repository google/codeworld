{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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
-- |
-- Module       : Data.MultiMap
-- Copyright    : (c) CodeWorld Authors 2017
-- License      : Apache-2.0
--
-- Maintainer   : Joachim Breitner <mail@joachim-breitner.de>
--
-- A simple MultiMap.
--
-- This differs from the one in the @multimap@ package by using
-- 'Data.Sequence.Seq' for efficient insert-at-end and other improved speeds.
--
-- Also only supports the operations required by CodeWorld for now.
{-# LANGUAGE TupleSections #-}

module Data.MultiMap
    ( MultiMap
    , empty
    , null
    , insertL
    , insertR
    , toList
    , spanAntitone
    , union
    , keys
    ) where

import Data.Bifunctor
import Data.Coerce
import qualified Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.Sequence as S
import Prelude hiding (null)

newtype MultiMap k v =
    MM (M.Map k (S.Seq v))
    deriving (Show, Eq)

empty :: MultiMap k v
empty = MM M.empty

null :: MultiMap k v -> Bool
null (MM m) = M.null m

insertL :: Ord k => k -> v -> MultiMap k v -> MultiMap k v
insertL k v (MM m) = MM (M.alter (Just . maybe (S.singleton v) (v S.<|)) k m)

insertR :: Ord k => k -> v -> MultiMap k v -> MultiMap k v
insertR k v (MM m) = MM (M.alter (Just . maybe (S.singleton v) (S.|> v)) k m)

toList :: MultiMap k v -> [(k, v)]
toList (MM m) = [(k, v) | (k, vs) <- M.toList m, v <- Data.Foldable.toList vs]

-- TODO: replace with M.spanAntitone once containers is updated
mapSpanAntitone :: (k -> Bool) -> M.Map k a -> (M.Map k a, M.Map k a)
mapSpanAntitone p =
    bimap M.fromDistinctAscList M.fromDistinctAscList .
    span (p . fst) . M.toList

spanAntitone :: (k -> Bool) -> MultiMap k v -> (MultiMap k v, MultiMap k v)
spanAntitone p (MM m) = coerce (mapSpanAntitone p m)

union :: Ord k => MultiMap k v -> MultiMap k v -> MultiMap k v
union (MM m1) (MM m2) = MM (M.unionWith (S.><) m1 m2)

keys :: MultiMap k v -> [k]
keys (MM m) = M.keys m
