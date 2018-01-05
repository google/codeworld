{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

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

module ErrorSanitizer ( filterOutput ) where

import           Data.Array (elems)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List
import           Data.Monoid
import           Text.Regex.Base
import           Text.Regex.TDFA
import           Text.Regex.TDFA.ByteString

replace :: ByteString -> ByteString -> ByteString -> ByteString
replace regex replacement str =
    let parts = concatMap elems $ (str =~ regex :: [MatchArray])
    in foldl replaceOne str (reverse parts)
  where
     replaceOne :: ByteString -> (Int, Int) -> ByteString
     replaceOne str (start, len) = pre <> replacement <> post
       where pre  = B.take start str
             post = B.drop (start + len) str

filterStages :: [(ByteString, ByteString)]
filterStages = [
        ("\xe2\x80\x98", ""),
        ("\xe2\x80\x99", ""),
        (" \\[-W[a-z-]*\\]", ""),
        ("IO action main", "variable program"),
        ("main IO action", "variable"),
        ("exported by", "defined in"),
        ("module Main", "your code"),
        ("main\\:Main", "your code"),
        ("is applied to too few arguments", "is missing arguments"),
        ("Couldn't match expected type Text\\s*with actual type GHC.Types.Char",
         "Text requires double quotes, rather than single."),
        ("base-[0-9.]*:GHC\\.Stack\\.Types\\.HasCallStack => ", ""),
        ("When checking that:\\s*[^\n]*\\s*is more polymorphic than:\\s*[^\n]*(\n\\s*)?",
         ""),
        ("\\[GHC\\.Types\\.Char\\] -> ", "\n"),
        ("base(-[0-9.]*)?\\:(.|\n)*->( |\n)*", "\n"),
        ("integer-gmp(-[0-9\\.]*)?:(.|\n)*->( |\n)*", ""),
        ("GHC\\.[A-Za-z.]*(\\s|\n)*->( |\n)*", ""),
        ("at src/[A-Za-z0-9\\/.:]*", ""),
        ("\\[GHC\\.Types\\.Char\\]" ,""),
        ("codeworld-base[-.:_A-Za-z0-9]*", "the standard library"),
        ("Main\\.", ""),
        ("main :: t", "program"),
        ("Prelude\\.", ""),
        ("\\bBool\\b", "Truth"),
        ("IO \\(\\)", "Program"),
        ("IO [a-z][a-zA-Z0-9_]*", "Program"),
        ("[ ]*Perhaps you intended to use TemplateHaskell\n", ""),
        ("imported from [^)\n]*", "defined in the standard library"),
        ("\\(and originally defined in [^)]*\\)", "\n"),
        ("the first argument", "the argument(s)"),
        ("[ ]*The function [a-zA-Z0-9_]* is applied to [a-z0-9]* arguments,\n", ""),
        ("[ ]*but its type .* has only .*\n", ""),
        ("A data constructor of that name is in scope; did you mean DataKinds\\?",
         "That name refers to a value, not a type."),
        ("type constructor or class", "type"),
        ("Illegal tuple section: use TupleSections",
         "This tuple is missing a value, or has an extra comma."),
        ("in string/character literal", "in text literal"),
        ("lexical error at character '\\\\822[01]'", "Smart quotes are not allowed."),
        ("[(]main[)]", "program"),
        ("\\bmain\\b", "program"),
        ("a pattern binding", "the definition"),
        ("Use -v to see a list of the files searched for\\.", ""),
        ("CallStack \\(from HasCallStack\\):", "When evaluating:"),
        ("\n\\s+\n", "\n")
    ]

filterOutput :: ByteString -> ByteString
filterOutput output = foldl' applyStage output filterStages
    where applyStage s (pattern, sub) = replace pattern sub s
