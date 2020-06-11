{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
module Internal.Text
  ( Text,
    fromString,
    toString,
    fromCWText,
    toCWText,
    (<>),
    numberOfCharacters,
    numberOfWords,
    numberOfLines,
    lines,
    unlines,
    words,
    unwords,
    characters,
    printed,
    joined,
    joinedWith,
    lowercase,
    uppercase,
    startsWith,
    endsWith,
    substitution,
    substitutions,
  )
where

import qualified Data.JSString as J
import qualified Data.JSString.Text as J
import Data.List (foldl')
import Data.Maybe
import qualified Data.Text as T
import Internal.Num
import Internal.Truth
import Numeric
import qualified "base" Prelude as P
import "base" Prelude ((.), Bool, String, length, map, show)

newtype Text = T
  { unT :: J.JSString
  }
  deriving (P.Eq)

{-# RULES
"equality/text" forall (x :: Text). (==) x = (P.==) x
  #-}

fromString :: String -> Text
fromString = T . J.pack

toString :: Text -> String
toString = J.unpack . unT

fromCWText :: Text -> T.Text
fromCWText = J.textFromJSString . unT

toCWText :: T.Text -> Text
toCWText = T . J.textToJSString

infixr 6 <>

(<>) :: Text -> Text -> Text
T a <> T b = T (J.append a b)

numberOfCharacters :: Text -> Number
numberOfCharacters = fromInt . J.length . unT

numberOfWords :: Text -> Number
numberOfWords = fromInt . length . J.words . unT

numberOfLines :: Text -> Number
numberOfLines = fromInt . length . J.lines . unT

lines :: Text -> [Text]
lines = map T . J.lines . unT

unlines :: [Text] -> Text
unlines = T . J.unlines . map unT

words :: Text -> [Text]
words = map T . J.words . unT

unwords :: [Text] -> Text
unwords = T . J.unwords . map unT

characters :: Text -> [Text]
characters = map (T . J.singleton) . J.unpack . unT

printed :: Number -> Text
printed = T . J.pack . show

joined :: [Text] -> Text
joined = T . J.concat . map unT

joinedWith :: ([Text], Text) -> Text
joinedWith (ts, T sep) = T (J.intercalate sep (map unT ts))

lowercase :: Text -> Text
lowercase = T . J.toLower . unT

uppercase :: Text -> Text
uppercase = T . J.toUpper . unT

startsWith :: (Text, Text) -> Truth
startsWith (T a, T b) = J.isPrefixOf b a

endsWith :: (Text, Text) -> Truth
endsWith (T a, T b) = J.isSuffixOf b a

-- | Gives the result of replacing one piece of text with another.
--
-- For example, @substitution("How do you do?", "do", "be")@ is equal to
-- @"How be you be?"@.
substitution :: (Text, Text, Text) -> Text
substitution (T text, T from, T to) = T (J.replace from to text)

-- | Gives the result of performing many substitutions in a piece of
-- text.  This is commonly used to build text to show in a program,
-- as in this example:
--
--    substitutions("Lives: [lives] of 3   Score: [score]",
--                  [("[lives]", printed(lives)),
--                   ("[score]", printed(score))])
substitutions :: (Text, [(Text, Text)]) -> Text
substitutions (T text, replacements) =
  T (foldl' (\a (T b, T c) -> J.replace b c a) text replacements)
