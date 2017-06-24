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
import System.IO

import "codeworld-compiler" Compile as C

main = do
    [src, out, err, mode] <- getArgs
    fileExists <- doesFileExist src
    if fileExists then do
        compileOutput <- extractSource src out err mode
        case compileOutput of
            True -> return () 
            False -> putStrLn "Some error occoured while compiling please check the error file"
        else putStrLn "File not found:"

extractSource :: String -> String -> String -> String -> IO Bool
extractSource  source out err mode = do 
    res <- C.compileSource source out err mode
    return res

