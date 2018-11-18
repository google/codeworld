{-# LANGUAGE BangPatterns #-}

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

module CodeWorld.Compile.Diagnostics where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.Exts (Module, SrcSpanInfo(..), SrcSpan(..), noSrcSpan)

type SourceMode = String  -- typically "codeworld" or "haskell"

type Diagnostic = (SrcSpanInfo, Level, String)

data Level = Info | Warning | Error deriving (Eq, Ord)

data ParsedCode = Parsed (Module SrcSpanInfo) | NoParse

formatLocation :: SrcSpanInfo -> String
formatLocation spn@(SrcSpanInfo s _)
  | spn == noSrcSpan = ""
  | otherwise        = "program.hs:" ++ show line ++ ":" ++ show col ++ ": "
  where
    fn = srcSpanFilename s
    line = srcSpanStartLine s
    col = srcSpanStartColumn s

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
