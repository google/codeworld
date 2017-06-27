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

import           Compile
import           Data.Char
import           Control.Monad
import           System.Directory
import           Test.HUnit             -- only import needed, others are optional

testcaseDir :: FilePath
testcaseDir = "codeworld-compiler/test/testcase"

testSourceFile :: String -> FilePath
testSourceFile testName = testcaseDir </> testName </> "source.hs"  

testErrorFile :: String -> FilePath
testErrorFile testName = testcaseDir </> testName </> "error.txt"

testSavedErrorFile :: String -> FilePath
testSavedErrorFile  testName = testcaseDir </> testName </> "saved_error.txt"

testOutputFile :: String -> FilePath
testOutputFile testName = testcaseDir </> testName </> "output.js"

savedErrorOutput :: String -> IO String
savedErrorOutput testName = do
    savedErrMsg <- readFile (testSavedErrorFile testName)
    return savedErrMsg

compileErrorOutput :: String -> IO String
compileErrorOutput testName = do 
    sourceFile <- readFile (testSourceFile testName)
    out <- compileSource 
        (testSourceFile testName)
        (testOutputFile testName)
        (testErrorFile  testName)
        "codeworld"
    errMsg <- readFile (testErrorFile testName)
    return errMsg

test1 = TestCase (do err1 <- (compileErrorOutput "test1")
                     err2 <- (compileErrorOutput "test2") 
                     assertEqual "Test case 1" err1 err2)
