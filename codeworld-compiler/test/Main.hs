{-# LANGUAGE OverloadedStrings #-}

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

import           Compile
import           Data.Char
import           Control.Monad
import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import           Test.HUnit             -- only import needed, others are optional

testcaseDir :: FilePath
testcaseDir = "test/testcase"

testcaseOutputDir :: FilePath
testcaseOutputDir = "../testcase-output"

testSourceFile :: String -> FilePath
testSourceFile testName = testcaseDir </> testName </> "source.hs"

testErrorFile :: String -> FilePath
testErrorFile testName = testcaseOutputDir </> testName </> "error.txt"

testSavedErrorFile :: String -> FilePath
testSavedErrorFile  testName = testcaseDir </> testName </> "saved_error.txt"

testOutputFile :: String -> FilePath
testOutputFile testName = testcaseOutputDir </> testName </> "output.js"

savedErrorOutput :: String -> IO String
savedErrorOutput testName = do
    savedErrMsg <- readFile (testSavedErrorFile testName)
    return savedErrMsg

compileErrorOutput :: String -> IO String
compileErrorOutput testName = do
    createDir <- createDirectoryIfMissing True (testcaseOutputDir </> testName)
    out <- compileSource
        FullBuild
        (testSourceFile testName)
        (testOutputFile testName)
        (testErrorFile  testName)
        "codeworld"
    errMsg <- readFile (testErrorFile testName)
    return errMsg

genTestCases :: [String] -> [Test]
genTestCases []  = ["Empty directory testcase"   ~: "FOo" ~=? (map toUpper "foo")]
genTestCases (x) = map toTestCase x

toTestCase x = x ~: do
    err1 <- compileErrorOutput x
    err2 <- savedErrorOutput x
    assertEqual x err1 err2

getTestCases :: FilePath -> IO Counts
getTestCases path = do
    dirContent <- getDirectoryContents path
    let filtered = filter (`notElem` ["..", "."]) dirContent
        cases =  genTestCases filtered
        testOutput = runTestTT (TestList cases)
    testOutput

main :: IO Counts
main = do
    cs@(Counts _ _ errs fails) <- getTestCases testcaseDir
    putStrLn (showCounts cs)
    if (errs > 0 || fails > 0)
        then exitFailure
        else exitSuccess
