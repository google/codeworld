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
module ParseCode
    ( checkParsedCode
    ) where

import Data.Function (on)
import Data.Generics
import Data.List (sortBy)
import Language.Haskell.Exts

checkParsedCode :: FilePath -> IO [String]
checkParsedCode src = do
    result <- parseFile src
    case result of
        ParseOk mod -> do
            let errs = sortBy (compare `on` fst) (getErrors mod)
            return [ formatLocation loc ++ err | (loc, err) <- errs ]
        ParseFailed _ _ -> return []

getErrors :: Module SrcSpanInfo -> [(SrcSpanInfo, String)]
getErrors m =
    everything (++) (mkQ [] badExps) m ++
    everything (++) (mkQ [] badMatches) m ++
    everything (++) (mkQ [] badPatterns) m

badExps :: Exp SrcSpanInfo -> [(SrcSpanInfo, String)]
badExps (App loc _ e)
    | not (isGoodExpRhs e) = [(loc, errorMsg)]
  where
    errorMsg = "warning: Missing parentheses in function application."
badExps _ = []

badMatches :: Match SrcSpanInfo -> [(SrcSpanInfo, String)]
badMatches (Match loc _ pats _ _) =
    [(loc, errorMsg) | p <- pats, not (isGoodPatRhs p)]
  where
    errorMsg = "warning: Missing parentheses in function application."
badMatches _ = []

badPatterns :: Pat SrcSpanInfo -> [(SrcSpanInfo, String)]
badPatterns (PApp loc _ pats) =
    [(loc, errorMsg) | p <- pats, not (isGoodPatRhs p)]
  where
    errorMsg = "warning: Missing parentheses in constructor application."
badPatterns _ = []

isGoodExpRhs :: Exp l -> Bool
isGoodExpRhs (Paren _ _) = True
isGoodExpRhs (Tuple _ _ _) = True
isGoodExpRhs _ = False

isGoodPatRhs :: Pat l -> Bool
isGoodPatRhs (PParen _ _) = True
isGoodPatRhs (PTuple _ _ _) = True
isGoodPatRhs _ = False

formatLocation :: SrcSpanInfo -> String
formatLocation (SrcSpanInfo s _) =
    "program.hs" ++ ":" ++ show line ++ ":" ++ show col ++ ": "
  where
    line = srcSpanStartLine s
    col = srcSpanStartColumn s
