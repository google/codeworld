{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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

import CodeWorld.Compile
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO.Temp
import Test.HUnit

compilerOutput :: String -> IO String
compilerOutput testName =
    withSystemTempDirectory "cwtest" $ \dir -> do
        compileSource
            (FullBuild (dir </> "result.js"))
            ("test/testcases" </> testName </> "source.hs")
            (dir </> "output.txt")
            "codeworld"
            False
        readFile (dir </> "output.txt")

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

toTestCase x =
    x ~: do
        putStrLn $ "Testing " ++ x ++ "..."
        actual <- trim <$> compilerOutput x
        expected <- trim <$> readFile ("test/testcases" </> x </> "expected_output.txt")
        assertEqual x expected actual

genTestCases :: [String] -> [Test]
genTestCases [] = ["Empty directory testcase" ~: "FOo" ~=? (map toUpper "foo")]
genTestCases x  = map toTestCase x

runTests :: FilePath -> IO Counts
runTests path = runTestTT . TestList . genTestCases =<< listDirectory path

main :: IO Counts
main = do
    cs@(Counts _ _ errs fails) <- runTests "test/testcases"
    putStrLn (showCounts cs)
    if errs > 0 || fails > 0 then exitFailure else exitSuccess
