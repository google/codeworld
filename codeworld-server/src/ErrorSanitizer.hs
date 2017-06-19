{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2017 The CodeWorld Authors. All rights reserved.

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

module ErrorSanitizer where

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Char
import 	         Data.List
import           Data.Tuple
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process
import           Text.Regex.TDFA hiding (Regex)
import           Text.Regex

import Util

filterStages :: [(Regex, String)]
filterStages = [
        (mkRegex "\226\8364\162", ""),
        (mkRegex "\226\8364\732", ""),
        (mkRegex "\226\8364\8482", ""),
        (mkRegex "\\’", ""),
        (mkRegex "IO action main", "variable main"),
        (mkRegex "module Main", "your program"),
        (mkRegex "main:Main", "your program"),
        (mkRegex "Couldn't match expected type Text\\s*with actual type GHC.Types.Char",
                "Text requires double quotes, rather than single."),
        (mkRegex "base-[0-9.]*:GHC\\.Stack\\.Types\\.HasCallStack => ", ""),
        (mkRegex "GHC\\.Types\\.Char" ,""),
        (mkRegex "codeworld-base[-.:_A-Za-z0-9]*", "the standard library"),
        (mkRegex "Main\\.", ""),
        (mkRegex "main :: t", "main program"),
        (mkRegex "Prelude\\.", ""),
        (mkRegex "IO \\(\\)", "Program"),
        (mkRegex "IO [a-z][a-zA-Z0-9_]*", "Program"),
        (mkRegex "\\bBool\\b", "Truth"),
        (mkRegex "[ ]*Perhaps you intended to use TemplateHaskell\n", ""),
        (mkRegex "imported from [^)\n]*", "defined in the standard library"),
        (mkRegex "the first argument", "the parameter(s)"),
        (mkRegex "[ ]*The function [a-zA-Z0-9_]* is applied to [a-z0-9]* arguments,\n", ""),
        (mkRegex "[ ]*but its type .* has only .*\n", ""),
        (mkRegex "A data constructor of that name is in scope; did you mean DataKinds\\?",
                "That name refers to a value, not a type."),
        (mkRegex "type constructor or class", "type"),
        (mkRegex "Illegal tuple section: use TupleSections",
                "This tuple is missing a value, or has an extra comma."),
        (mkRegex "Use -v to see a list of the files searched for\\.", ""),
        (mkRegex "\n\\s+\n", "\n"),
        (mkRegex "When checking that:\\s*[^\n]*\\s*is more polymorphic than:\\s*[^\n]*(\n\\s*)?",
                ""),
        (mkRegex "\\[GHC\\.Types\\.Char\\] -> ", "\n"),
        (mkRegex "\\(and originally defined in [^)]*\\)", "\n"),
        (mkRegex "in string\\/character literal", "in text literal"),
        (mkRegex "CallStack \\(from HasCallStack\\):", "When Evaluating:"),
        (mkRegex "at src\\/[A-Za-z0-9\\/.:]*", ""),
        (mkRegex "GHC\\.[A-Za-z.]*(\\s|\n)*->( |\n)*", ""),
        (mkRegex "integer-gmp(-[0-9\\.]*)?:(.|\n)*?->( |\n)*", ""),
        (mkRegex "base(-[0-9.]*)?\\:(.|\n)*?->( |\n)*", "\n"),
        (mkRegex "lexical error at character '\822[01]'", "Smart quotes are not allowed.")
    ]

filterOutput :: ByteString -> ByteString
filterOutput output = 
    let out = foldl' applyStage (C.unpack output) filterStages
    in (C.pack out)
    where applyStage s (pattern, sub) = subRegex pattern s sub
