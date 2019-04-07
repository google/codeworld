{-# LANGUAGE OverloadedStrings #-}

module Driver (
    tests
    ) where

import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?))
import Test.Framework.Providers.HUnit (testCase)
import CodeWorld


tests :: Test
tests = testGroup "Driver" [
    testCase "time step elision" timeStepElision
    ]

timeStepElision = 0 @=? 1
