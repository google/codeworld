{-
  Copyright 2014 Google Inc. All rights reserved.

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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Data.List
import System.Environment
import System.IO
import Text.Regex.TDFA

main :: IO ()
main = do
    [symbolFile] <- getArgs
    decls <- mergeContinued <$> lines <$> readFile symbolFile
    let preludeDecls = restrictToPrelude decls
    let tokens = concatMap tokensFrom preludeDecls
    mapM_ putStrLn (sort $ nub $ tokens)

restrictToPrelude :: [String] -> [String]
restrictToPrelude = takeWhile (not . isModule)
                  . tail
                  . dropWhile (not . isPrelude)
  where isModule  decl = decl =~ "^module .*$" :: Bool
        isPrelude decl = decl == "module Prelude"

mergeContinued :: [String] -> [String]
mergeContinued []      = []
mergeContinued [l]     = [l]
mergeContinued (l1:l2:r)
    | take 1 l2 == " " = mergeContinued ((l1 ++ " " ++ dropWhile (== ' ') l2) : r)
    | otherwise        = l1 : mergeContinued (l2:r)

tokensFrom :: String -> [String]
tokensFrom decl = tokensFromOperator decl ++
                  tokensFromIdentifier decl ++
                  tokensFromData decl ++
                  tokensFromNewtype decl ++
                  tokensFromType decl

tokensFromOperator :: String -> [String]
tokensFromOperator decl = concatMap (elemsAt [1]) matches
  where matches = decl =~ "^\\(([^)]*)\\) :: .*$" :: [[String]]

tokensFromIdentifier :: String -> [String]
tokensFromIdentifier decl = concatMap (elemsAt [1]) matches
  where matches = decl =~ "^([A-Za-z0-9_]*) :: .*$" :: [[String]]

tokensFromData :: String -> [String]
tokensFromData decl = concatMap (elemsAt [1]) matches
  where matches = decl =~ "^data ([A-Za-z0-9_]*)( .*)?$" :: [[String]]

tokensFromNewtype :: String -> [String]
tokensFromNewtype decl = concatMap (elemsAt [1]) matches
  where matches = decl =~ "^newtype ([A-Za-z0-9_]*)( .*)?$" :: [[String]]

tokensFromType :: String -> [String]
tokensFromType decl = concatMap (elemsAt [1]) matches
  where matches = decl =~ "^type ([A-Za-z0-9_]*)( .*)?$" :: [[String]]

-- Selects specific indices from a list.  The list of indices must be
-- in non-decreasing order.
elemsAt :: [Int] -> [a] -> [a]
elemsAt is xs = go xs is 0
  where go []     _      _  = []
        go _      []     _  = []
        go (x:xs) (i:is) !j -- invariant: i >= j
          | i == j          = x : go (x:xs) is j
          | otherwise       = go xs (i:is) (j+1)
