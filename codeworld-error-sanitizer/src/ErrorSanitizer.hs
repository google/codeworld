{-# LANGUAGE OverloadedStrings #-}

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
module ErrorSanitizer (rewriteErrors) where

import Data.List
import Data.Text (Text)
import RegexShim (replace)

rewriteStages :: [(Text, Text)]
rewriteStages =
    [ ("\8216", "")
    , ("\8217", "")
    , ("warning: ([a-zA-Z,0-9 ]*)\\[-Wdefer[a-z-]*\\]", "error: \\1")
    , (" \\[-W[a-z-]*\\]", "")
    , ("IO action main", "variable program")
    , ("main IO action", "variable")
    , ("exported by", "defined in")
    , ("module Main", "your code")
    , ("main\\:Main", "your code")
    , ("[ ]*\8226 Possible cause: [(].*[)] is applied to too many arguments\n", "")
    , ("[ ]*except perhaps to import instances from [A-Za-z0-9.]*\n", "")
    , ("[ ]*To import instances alone, use: import [A-Za-z0-9.]*[(][)]\n", "")
    , ("^([ ]*(\8226 )?)(.*[Nn]ot in scope: .* :: Text)",
       "\\1\\3\n\\1Perhaps you meant to put quotation marks around this text.")
    , ("is applied to too few arguments", "is missing arguments")
    , ("is applied to too many arguments", "is a value, but a function is needed here.")
    , ("Couldn't match expected type Text\\s*with actual type GHC.Types.Char",
       "Text requires double quotes, rather than single.")
    , ("[ ]*Perhaps you intended to use TemplateHaskell or TemplateHaskellQuotes\n", "")
    , ("In the Template Haskell quotation '.*'",
       "Use double quotes around text values.")
    , ("[ ]+\8226 In ([^\n\8226]|\n )+\n", "")
    , ("base-[0-9.]*:GHC\\.Stack\\.Types\\.HasCallStack => ", "")
    , ("When checking that:\\s*[^\n]*\\s*is more polymorphic than:\\s*[^\n]*(\n\\s*)?",
       "")
    , ("Perhaps you need a 'let' in a 'do' block[?][ \t\n]*e.g. '[^']*' instead of '[^']*'",
       "An equation does not fit here. Is this line indented incorrectly?")
    , ("\\[GHC\\.Types\\.Char\\] -> ", "\n")
    , ("base(-[0-9.]*)?\\:(.|\n)*->( |\n)*", "\n")
    , ("integer-gmp(-[0-9\\.]*)?:(.|\n)*->( |\n)*", "")
    , ("GHC\\.[A-Za-z.]*(\\s|\n)*->( |\n)*", "")
    , ("at src/[A-Za-z0-9\\/.:]*", "")
    , ("\\[GHC\\.Types\\.Char\\]", "")
    , ("codeworld-base[-.:_A-Za-z0-9]*", "the standard library")
    , ("Main\\.", "")
    , ("main :: t", "program")
    , ("Prelude\\.", "")
    , ("\\bBool\\b", "Truth")
    , ("IO \\(\\)", "Program")
    , ("IO [a-z][a-zA-Z0-9_]*", "Program")
    , ("[ ]*Perhaps you intended to use TemplateHaskell\n", "")
    , ("imported from [^)\n]*", "defined in the standard library")
    , ("[ ]*[(]and originally defined in [^)]*[)]\n", "")
    , ("the first argument", "the argument(s)")
    , ("[ ]*The function [a-zA-Z0-9_]* is applied to [a-z0-9]* arguments,\n",
       "")
    , ("[ ]*but its type .* has only .*\n", "")
    , ("A data constructor of that name is in scope; did you mean DataKinds\\?",
       "That name refers to a value, not a type.")
    , ("type constructor or class", "type")
    , ("Illegal tuple section: use TupleSections",
       "This tuple is missing a value, or has an extra comma.")
    , ("in string/character literal", "in text literal")
    , ("lexical error in text literal at end of input",
       "Missing the end quotation mark for a Text value.")
    , ("lexical error in text literal at character '\\\\n'",
       "Missing the end quotation mark for a Text value.")
    , ("lexical error at character '\\\\822[01]'",
       "Smart quotes are not allowed.")
    , ("[(]main[)]", "program")
    , ("\\bmain\\b", "program")
    , ("a pattern binding", "the definition")
    , ("Use -v to see a list of the files searched for\\.", "")
    , ("[(]Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits[)]", "...")
    , ("CallStack \\(from HasCallStack\\):", "When evaluating:")
    , (", called at program.hs:", ", used at program.hs:")
    , ("module header, import declaration\n    or top-level declaration expected.",
       "An equation was expected here.\n    Are you missing the equal sign?")
    , ("\n\\s+\n", "\n")
    ]

rewriteErrors :: Text -> Text
rewriteErrors output = foldl' applyStage output rewriteStages
  where
    applyStage s (pattern, sub) = replace pattern sub s
