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

module ParseCode (checkParsedCode) where

import qualified Data.ByteString as B
import           Data.ByteString.Char8 (pack)
import           Data.List.Split (splitOn)
import           Data.Generics
import           Language.Haskell.Exts

checkParsedCode :: FilePath -> FilePath -> IO Bool
checkParsedCode src err = do
    result <- parseFile src
    let parseList = words $ show result
        parseresult = head parseList
    if parseresult == "ParseFailed" then do
        source <- readFile src
        let sourceSplitList = splitOn "\n" source
            errLin = errLineLocation parseList
            errCol = errColumnLocation parseList
        B.writeFile err (pack ("ParseError, Error at: program.hs:"
                ++ (show $ errLin + 1) ++ ":" ++ (show errCol)
                ++ "\n>  " ++ (sourceSplitList !! errLin)))
        return False
        else do
            let outp = getErrors (getSrcSpanInfo result)
            if outp == [] then return True
                else do
                    let outpString = tail $ tail $ init $ init (show outp)
                    B.writeFile err (pack outpString)
                    return False

getSrcSpanInfo :: ParseResult (Module SrcSpanInfo) -> Module SrcSpanInfo
getSrcSpanInfo (ParseOk x) = x

getErrors :: Module SrcSpanInfo -> [String]
getErrors m = everything (++) (mkQ [] badExps) m ++
              everything (++) (mkQ [] badMatches) m
              -- everything (++) (mkQ [] badInfixExps) m

{-
badInfixExps :: Exp SrcSpanInfo -> [String]
badInfixExps (InfixApp a (App l _ _) c d) = ["Error at InfixApp: Missing &"
        ++ "program.hs:"
        ++ (show $ parseSrcSpanStartLine a)
        ++ ":"
        ++ (show $ parseSrcSpanStartColumn a)]
badInfixExps (InfixApp a b c (App l _ _)) = ["Error at InfixApp: Missing &"
        ++ "program.hs:"
        ++ (show $ parseSrcSpanStartLine l)
        ++ ":"
        ++ (show $ parseSrcSpanStartColumn l)]
badInfixExps (InfixApp a b c d) | not (isGoodInfixExpRhs c) = ["Error at InfixApp: program.hs:"
        ++ (show $ parseSrcSpanStartLine a)
        ++ ":"
        ++ (show $ parseSrcSpanStartColumn a)]
badInfixExps _                                              = []
-}

{-
badExps (App x (Lit m _) (Lit m _))  = []
badExps (App x (Lit m _) (Var m _))  = []
-}
badExps :: Exp SrcSpanInfo -> [String]
badExps (App x (Var l _) e) | not (isGoodExpRhs e) = [
        "Missing parantheses in function applicaion at: program.hs:"
        ++ (show $ parseSrcSpanEndLine l) ++ ":"
        ++ (show $ parseSrcSpanEndColumn l)]
badExps _                                  = []

badMatches :: Match SrcSpanInfo -> [String]
badMatches (Match _ _ pats _ _) = [ "Bad pattern match!" | p <- pats, not (isGoodPatRhs p) ]
badMatches _                    = []

isGoodExpRhs :: Exp l -> Bool
isGoodExpRhs (Paren _ _)   = True
isGoodExpRhs (Tuple _ _ _) = True
isGoodExpRhs _             = False

isGoodInfixExpRhs :: QOp l -> Bool
isGoodInfixExpRhs (QVarOp _ _) = True
isGoodInfixExpRhs _            = False

isGoodPatRhs :: Pat l -> Bool
isGoodPatRhs (PParen _ _)   = True
isGoodPatRhs (PTuple _ _ _) = True
isGoodPatRhs _              = False

parseSrcSpanEndLine :: SrcSpanInfo -> Int
parseSrcSpanEndLine (SrcSpanInfo x _) = srcSpanEndLine x

parseSrcSpanEndColumn :: SrcSpanInfo -> Int
parseSrcSpanEndColumn (SrcSpanInfo x _) = srcSpanEndColumn x

parseSrcSpanStartLine :: SrcSpanInfo -> Int
parseSrcSpanStartLine (SrcSpanInfo x _) = srcSpanStartLine x

parseSrcSpanStartColumn :: SrcSpanInfo -> Int
parseSrcSpanStartColumn (SrcSpanInfo x _) = srcSpanStartColumn x

errLineLocation :: [String] -> Int
errLineLocation parseList = errLin
    where errLinChar = parseList !! 3
          errLinM = read errLinChar :: Int
          errLin = errLinM - 1

errColumnLocation :: [String] -> Int
errColumnLocation parseList = errCol
    where errColChar = parseList !! 4
          errColM = take 1 errColChar
          errCol = read errColM :: Int
