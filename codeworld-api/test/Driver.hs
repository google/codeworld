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
    testCase "wrapping unshared identity is unshared" wrappedStepDropsUnsharedIdentity,
    testCase "wrapping shared identity is shared" wrappedStepSavesSharedIdentity
    ]

wrappedStepSavesSharedIdentity :: Assertion
wrappedStepSavesSharedIdentity = do
    let wrapped = wrappedInitial 42
    initialName <- makeStableName $! wrapped
    sharedName  <- makeStableName $! (wrappedStep (const id) 1 wrapped)
    assertBool "" $ initialName == sharedName

wrappedStepDropsUnsharedIdentity :: Assertion
wrappedStepDropsUnsharedIdentity = do
    let wrapped = wrappedInitial 42
        unsharedId x = (x + 1) - 1
    initialName <- makeStableName $! wrapped
    unsharedName <- makeStableName $! (wrappedStep (const unsharedId) 1 wrapped)
    assertBool "" $ initialName /= unsharedName
