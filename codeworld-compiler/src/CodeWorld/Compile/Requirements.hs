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
import Language.Haskell.Exts (Module, SrcSpanInfo, noSrcSpan)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

pattern :: Text
pattern = "{-+[[:space:]]*(REQUIRES\\b(\n|[^-]|-[^}])*)-}"

requirementsDiagnostics :: SourceMode -> Text -> ParsedCode -> [Diagnostic]
requirementsDiagnostics _ src m
  | hasReqs   = [(noSrcSpan, Info, response)]
  | otherwise = []
  where reqs = [ parseRequirement $ T.take len (T.drop off src)
                 | matchArray :: MatchArray <- src =~ pattern
                 , rangeSize (bounds matchArray) > 1
                 , let (off, len) = matchArray ! 1 ]
        hasReqs = not (null (reqs))
        response = ":: REQUIREMENTS ::\n" ++
                   concatMap (handleRequirement m) reqs ++
                   ":: END REQUIREMENTS ::\n"

handleRequirement :: ParsedCode -> Either String Requirement -> String
handleRequirement m (Left err) = "[?] A requirement could not be understood\n" ++ err
handleRequirement m (Right r) = label ++ desc ++ "\n" ++ concat [ msg ++ "\n" | msg <- msgs ]
  where (desc, success, msgs) = evalRequirement r m
        label | success   = "[Y] "
              | otherwise = "[N] "
