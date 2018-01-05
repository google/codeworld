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

module ParseCode (checkParsedCode) where

import           Data.Generics
import           Language.Haskell.Exts

checkParsedCode :: FilePath -> FilePath -> IO Bool
checkParsedCode src err = do
    result <- parseFile src
    case result of
        ParseOk mod -> case getErrors mod of
            [] -> return True
            errors -> writeFile err (concatMap (++ "\n\n") errors) >> return False
        ParseFailed _ _ -> return True

getErrors :: Module SrcSpanInfo -> [String]
getErrors m = everything (++) (mkQ [] badExps) m ++
              everything (++) (mkQ [] badMatches) m ++
              everything (++) (mkQ [] badPatterns) m


badExps :: Exp SrcSpanInfo -> [String]
badExps (App x _ e) | not (isGoodExpRhs e) =
    [ formatLocation x ++ errorMsg]
    where errorMsg = "error: Missing parentheses in function application"
badExps _                                  = []

badMatches :: Match SrcSpanInfo -> [String]
badMatches (Match l _ pats _ _) =
    [ formatLocation l ++ errorMsg | p <- pats, not (isGoodPatRhs p) ]
    where errorMsg = "error: Missing parentheses in function application"
badMatches _                    = []

badPatterns :: Pat SrcSpanInfo -> [String]
badPatterns (PApp l _ pats) =
    [ formatLocation l ++ errorMsg | p <- pats, not (isGoodPatRhs p) ]
    where errorMsg = "error: Missing parentheses in constructor application"
badPatterns _               = []

isGoodExpRhs :: Exp l -> Bool
isGoodExpRhs (Paren _ _)   = True
isGoodExpRhs (Tuple _ _ _) = True
isGoodExpRhs _             = False

isGoodPatRhs :: Pat l -> Bool
isGoodPatRhs (PParen _ _)   = True
isGoodPatRhs (PTuple _ _ _) = True
isGoodPatRhs _              = False

formatLocation :: SrcSpanInfo -> String
formatLocation (SrcSpanInfo s _) =
        "program.hs" ++ ":" ++ show line ++ ":" ++ show col ++ ": "
    where line = srcSpanStartLine s
          col = srcSpanStartColumn s
