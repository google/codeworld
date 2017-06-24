{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}


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


import System.Environment
import System.Directory

import "codeworld-compiler" Compile as C

main = do
    a <- getArgs
    if length a <= 3
        then print "Insufficient args, atleast 4 required(source, output, error, buildargs)"
        else do
            fileExists <- doesFileExist (head a)
            if fileExists then do
                let src = head a
                    out = a !! 1 
                    err = a !! 2
                    arg = drop 3 a
                compileOutput <- extractSource src out err arg
                case compileOutput of
                    True -> putStrLn "Done please check your output file"
                    False -> putStrLn "Some error occoured while compiling please check the error file"
                else print "Wrong source file address please check it once again"

extractSource :: String -> String -> String -> [String] -> IO Bool
extractSource  source out err arg = doesFileExist source >>= \a ->
    if a then do
        res <- C.compileSource source out err arg
        return res
    else return False

