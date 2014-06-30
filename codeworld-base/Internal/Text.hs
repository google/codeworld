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
    append,
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
    join,
    replace,
    T.toLower,
    T.toUpper,
    T.strip,
    stripPrefix,
    stripSuffix,
    search,
    substring
    ) where

import qualified "base" Prelude as P
import "base" Prelude (Bool, (.))
import "base" Data.Maybe

import Data.Text (Text)
import qualified Data.Text as T

import Internal.Num

fromString :: P.String -> Text
fromString = T.pack

empty :: Text -> Bool
empty = T.null

append :: (Text, Text) -> Text
append (a, b) = T.append a b

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

show :: Number -> Text
show = T.pack . P.show

join :: ([Text], Text) -> Text
join (ts, sep) = T.intercalate sep ts

-- | Replaces one piece of text with another.
--
-- For example, `replace("How do you do?", "do", "be")` is equal to `"How be you be?"`.
replace :: (Text, Text, Text) -> Text
replace (text, from, to) = T.replace from to text

-- | Removes a prefix from some text.
--
-- For example, `stripPrefix("Dr. Jones", "Dr. ")` is equal to `"Jones"`.
-- If the prefix isn't there, the result is the same string, unchanged.
stripPrefix :: (Text, Text) -> Text
stripPrefix (text, pfx) = fromMaybe text (T.stripPrefix pfx text)

-- | Removes a suffix from some text.
--
-- For example, `stripSuffix("smallest", "est")` is equal to `"small"`.
-- If the suffix isn't there, the result is the same string, unchanged.
stripSuffix :: (Text, Text) -> Text
stripSuffix (text, sfx) = fromMaybe text (T.stripSuffix sfx text)

-- | Finds all indices where some text appears in a larger piece of text.
--
-- For example, `search("How do you do?", "do")` is equal to the list `[4, 11]`.
-- Indices start at zero.
search :: (Text, Text) -> [Number]
search (haystack, needle) = P.map (fromInt . T.length . P.fst) (T.breakOnAll needle haystack)

-- | Takes part of a string at a starting index and length.
--
-- For example, `substring("funny", 2, 2)` is equal to `"nn"`.
-- Indices start at zero.
substring :: (Text, Number, Number) -> Text
substring (text, start, length) = T.take (toInt length) (T.drop (toInt start) text)
