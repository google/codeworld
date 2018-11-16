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
  where results = [ (noSrcSpan, parseRequirement ln col chunk)
                    | matchArray :: MatchArray <- src =~ pattern
                    , rangeSize (bounds matchArray) > 1
                    , let (off, len) = matchArray ! 1
                    , let srcSpanInfo = srcSpanFor src off len
                    , let SrcSpanInfo spn _ = srcSpanInfo
                    , let ln = srcSpanStartLine spn
                    , let col = srcSpanStartColumn spn
                    , let chunk = T.take len (T.drop off src) ]
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
  where (_, ln1, col1) = T.foldl' next (off, 1, 1) pre
        (_, ln2, col2) = T.foldl' next (len, ln1, col1) mid

        (pre, post) = T.splitAt off src
        mid = T.take len post

        next (!n, !ln, !col) '\r' = (n - 1, ln, col)
        next (!n, !ln, !col) '\n' = (n - 1, ln + 1, 1)
        next (!n, !ln, !col) '\t' = (n - 1, ln, col + 8 - (col - 1) `mod` 8)
        next (!n, !ln, !col) _    = (n - 1, ln, col + 1)
