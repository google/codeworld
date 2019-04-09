{-# LANGUAGE OverloadedStrings #-}

module Driver (
    tests
    ) where

import Test.Framework (Test, testGroup)
import Test.HUnit hiding (Test)
import Test.Framework.Providers.HUnit (testCase)
import CodeWorld
import CodeWorld.Driver
import CodeWorld.Event
import CodeWorld.CanvasM
import System.Mem.StableName
import Control.Concurrent

tests :: Test
tests = testGroup "Driver" [
    testCase "modifyMVarIfDifferent" modifyMVarIfDifferentTest
    ]

modifyMVarIfDifferentTest :: Assertion
modifyMVarIfDifferentTest = do
    let initial = 0
    mv <- newMVar initial
    initialName <- makeStableName $! initial
    modifyMVarIfDifferent mv (\n -> n + 1 - 1)
    res <- readMVar mv
    resName <- makeStableName $! res
    assertBool "modifyMVarIfDifferent" (resName == initialName)

