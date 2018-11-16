{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module CodeWorld.Compile.Requirements (requirementsDiagnostics) where

import CodeWorld.Compile.Diagnostics
import CodeWorld.Compile.Requirements.Eval
import CodeWorld.Compile.Requirements.Language
import Data.Array
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.Exts
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

requirementsDiagnostics :: SourceMode -> Text -> ParsedCode -> [Diagnostic]
requirementsDiagnostics _ src m
  | null reqs = diags
  | otherwise = (noSrcSpan, Info,
                 ":: REQUIREMENTS ::\n" ++
                 concatMap (handleRequirement m) reqs ++
                 ":: END REQUIREMENTS ::\n") : diags
  where (diags, reqs) = extractRequirements src

pattern :: Text
pattern = "{-+[[:space:]]*(REQUIRES\\b(\n|[^-]|-[^}])*)-}"

extractRequirements :: Text -> ([Diagnostic], [Requirement])
extractRequirements src = (diags, reqs)
  where results = [ (srcSpanFor src off len,
                     parseRequirement (T.take len (T.drop off src)))
                    | matchArray :: MatchArray <- src =~ pattern
                    , rangeSize (bounds matchArray) > 1
                    , let (off, len) = matchArray ! 1 ]
        diags = [ (srcSpan, Warning,
                   "The requirement could not be understood:\n" ++ err)
                  | (srcSpan, Left err) <- results ]
        reqs = [ req | (_, Right req) <- results ]

handleRequirement :: ParsedCode -> Requirement -> String
handleRequirement m r =
    label ++ desc ++ "\n" ++ concat [ "    " ++ msg ++ "\n" | msg <- msgs ]
  where (desc, success, msgs) = evalRequirement r m
        label | success   = "[Y] "
              | otherwise = "[N] "

srcSpanFor :: Text -> Int -> Int -> SrcSpanInfo
srcSpanFor src off len =
    SrcSpanInfo (SrcSpan "program.hs" ln1 col1 ln2 col2) []
  where (ln1, col1) = advance 0 off 1 1
        (ln2, col2) = advance off len ln1 col1

        advance _ !0 !ln !col = (ln, col)
        advance !i !n !ln !col = case T.index src n of
            '\r' -> advance (i+1) (n-1) ln col
            '\n' -> advance (i+1) (n-1) (ln + 1) 1
            '\t' -> advance (i+1) (n-1) ln (col + 8 - (mod (col - 1) 8))
            _    -> advance (i+1) (n-1) ln (col + 1)
