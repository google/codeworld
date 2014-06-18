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

module Internal.Text (
    Text,
    fromString,
    empty,
    T.append,
    (<>),
    appendAll,
    numberOfCharacters,
    numberOfWords,
    numberOfLines,
    characters,
    T.lines,
    T.words,
    T.unlines,
    T.unwords,
    show,
    T.intercalate,
    T.replace,
    T.toLower,
    T.toUpper,
    T.strip,
    T.stripPrefix,
    T.stripSuffix,
    search,
    substring
    ) where

import qualified "base" Prelude as P
import "base" Prelude (Bool, (.))

import Data.Text (Text)
import qualified Data.Text as T

import Internal.Num

fromString :: P.String -> Text
fromString = T.pack

numberOfCharacters :: Text -> Number
numberOfCharacters = fromInt . T.length

numberOfWords :: Text -> Number
numberOfWords = fromInt . P.length . T.words

numberOfLines :: Text -> Number
numberOfLines = fromInt . P.length . T.lines

characters :: Text -> [Text]
characters = P.map T.singleton . T.unpack

infixr 6 <>
(<>) :: Text -> Text -> Text
(<>) = T.append

appendAll :: [Text] -> Text
appendAll = T.concat

empty :: Text -> Bool
empty = T.null

show :: Number -> Text
show = T.pack . P.show

search :: Text -> Text -> [Number]
search needle haystack = P.map (fromInt . T.length . P.fst) (T.breakOnAll needle haystack)

substring :: Number -> Number -> Text -> Text
substring start length = T.take (toInt length) . T.drop (toInt start)
