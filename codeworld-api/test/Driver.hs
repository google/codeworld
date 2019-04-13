{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

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
import GHC.Prim

tests :: Test
tests = testGroup "Driver" [
    testCase "wrapping of shared identity is shared" wrappedStepSavesSharedIdentity,
    testCase "pointer does not change if updating value is pure" pureIdentityPointerChange,
    testCase "pointer does not change if updating value is using MVar" mvarPointerChange
    ]

identical :: a -> a -> Bool
identical !x !y = case reallyUnsafePtrEquality# x y of
    0# -> False
    _  -> True

wrappedStepSavesSharedIdentity :: Assertion
wrappedStepSavesSharedIdentity = do
    let wrapped = wrappedInitial 42
    assertBool "" $ identical wrapped (wrappedStep (const id) 1 wrapped)

pureIdentityPointerChange :: Assertion
pureIdentityPointerChange = do
    let initial = 0
        target = initial + 1 - 1
    assertBool "" $ identical initial target

mvarPointerChange :: Assertion
mvarPointerChange = do
    let initial = 0 :: Integer
    mvar <- newMVar initial
    modifyMVar_ mvar (\x -> return $ x + 1 - 1)
    target <- readMVar mvar
    assertBool "" $ identical initial target
