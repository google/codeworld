{-# LANGUAGE OverloadedStrings #-}

module TestDriver (
    test_timeStepElision
    ) where

import Test.HUnit
import CodeWorld


test_timeStepElision = TestCase $
    do
        let newStateName = 0
            oldStateName = 1
        assertBool "Is time step ellision ok " $ newStateName == oldStateName
