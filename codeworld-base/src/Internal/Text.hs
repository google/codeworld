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

module Internal.Text (
    Text,
    fromString,
    (<>),
    numberOfCharacters,
    numberOfWords,
    numberOfLines,
    T.lines,
    T.unlines,
    T.words,
    T.unwords,
    characters,
    printed,
    show,
    joined,
    joinedWith,
    lowercase,
    uppercase,
    capitalized,
    startsWith,
    endsWith,
    substitution,
    substitutions
    ) where

import qualified "base" Prelude as P
import "base" Prelude (Bool, (.))
import "base" Data.Maybe
import "base" Data.List (foldl')
import "base" Numeric

import Data.Text (Text)
import qualified Data.Text as T

import Internal.Num

fromString :: P.String -> Text
fromString = T.pack

infixr 6 <>
(<>) :: Text -> Text -> Text
(<>) = T.append

numberOfCharacters :: Text -> Number
numberOfCharacters = fromInt . T.length

numberOfWords :: Text -> Number
numberOfWords = fromInt . P.length . T.words

numberOfLines :: Text -> Number
numberOfLines = fromInt . P.length . T.lines

characters :: Text -> [Text]
characters = P.map T.singleton . T.unpack

printed :: Number -> Text
printed = T.pack . P.show

show :: Number -> Text
show = printed
{-# WARNING show "Please use printed(...) instead of show(...)" #-}

joined :: [Text] -> Text
joined = T.concat

joinedWith :: ([Text], Text) -> Text
joinedWith (ts, sep) = T.intercalate sep ts

lowercase :: Text -> Text
lowercase = T.toLower

uppercase :: Text -> Text
uppercase = T.toUpper

capitalized :: Text -> Text
capitalized = T.toTitle

startsWith :: (Text, Text) -> Bool
startsWith (a, b) = T.isPrefixOf b a

endsWith :: (Text, Text) -> Bool
endsWith (a, b) = T.isSuffixOf b a

-- | Gives the result of replacing one piece of text with another.
--
-- For example, `substitution("How do you do?", "do", "be")` is equal to
-- `"How be you be?"`.
substitution :: (Text, Text, Text) -> Text
substitution (text, from, to) = T.replace from to text

-- | Gives the result of performing many substitutions in a piece of
-- text.  This is commonly used to build text to show in a program,
-- as in this example:
--
--    substitutions("Lives: [lives] of 3   Score: [score]",
--                  [("[lives]", printed(lives)),
--                   ("[score]", printed(score))])
substitutions :: (Text, [(Text, Text)]) -> Text
substitutions (text, replacements) =
    foldl' (\ a (b, c) -> T.replace b c a) text replacements
