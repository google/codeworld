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

import Control.Applicative
import Data.List
import System.Environment
import System.IO
import Text.Regex.TDFA

main :: IO ()
main = do
    [symbolFile] <- getArgs
    decls <- mergeContinued <$> lines <$> readFile symbolFile
    let tokens = filter (not . (`elem` blacklist)) $ concatMap tokensFrom decls
    mapM_ putStrLn (sort $ nub $ keywords ++ whitelist ++ tokens)

blacklist :: [String]
blacklist = [
    "IO",
    "fromDouble",
    "fromInt",
    "fromInteger",
    "fromRational",
    "fromString",
    "ifThenElse",
    "toDouble",
    "toInt"
    ]

whitelist :: [String]
whitelist = ["main"]

mergeContinued :: [String] -> [String]
mergeContinued []      = []
mergeContinued [l]     = [l]
mergeContinued (l1:l2:r)
    | take 1 l2 == " " = mergeContinued ((l1 ++ " " ++ dropWhile (== ' ') l2) : r)
    | otherwise        = l1 : mergeContinued (l2:r)

tokensFrom :: String -> [String]
tokensFrom decl = filter ((> 1) . length)
                $ concatMap (everyOther . tail) (decl =~ expr :: [[String]])
  where expr = "^(([A-Za-z0-9_]*) :: .*)"
            ++ "|(\\(([^)]*)\\) :: .*)"
            ++ "|(data ([A-Za-z0-9_]*) .*= .*)"
            ++ "|(newtype ([A-Za-z0-9_]*) .*= .*)"
            ++ "|(type ([A-Za-z0-9_]*) .*= .*)"
            ++ "$"

everyOther :: [a] -> [a]
everyOther (a:b:c) = b : everyOther c
everyOther _       = []

keywords :: [String]
keywords = [
    "--",      "{-",      "-}",      "::",      "->",      "<-",
    "..",      "case",    "of",      "if",      "then",    "else",
    "data",    "let",     "in",      "where"
    ]
