{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2020 The CodeWorld Authors. All rights reserved.

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

import Data.Text (Text)
import RegexShim
import Test.Framework (Test, defaultMain, testGroup)
import Test.HUnit hiding (Test)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain [allTests]

allTests :: Test
allTests = testGroup "RegexShim"
    [
        testCase "replaces groups" $ testReplacesGroups,
        testCase "replaces multiple occurrences" $ testReplacesMultiGroups
    ]

testReplacesGroups :: Assertion
testReplacesGroups = do
    let result = replace "a(b*)c(d*)e" "x\\2y\\1z" "abbbcdddde"
    assertEqual "result" "xddddybbbz" result

testReplacesMultiGroups :: Assertion
testReplacesMultiGroups = do
    let result = replace "a(b*)c(d*)e" "x\\1y\\2y\\1z" "abbbcddde"
    assertEqual "result" "xbbbydddybbbz" result
