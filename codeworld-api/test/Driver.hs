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
tests = testGroup "Driver"
    [ testCase "toState preserves identity" $ do
        let wrapped = wrappedInitial 42
        let target = toState id wrapped
        assertBool "" $ identical wrapped target
    , testCase "wrappedStep preserves identity" $ do
        -- Expected failure: See https://github.com/google/codeworld/issues/681
        let wrapped = wrappedInitial 42
        let target  = wrappedStep (const id) 1 wrapped
        assertBool "" $ not $ identical wrapped target
    , testCase "wrapping of shared identity is shared (events)" $ do
        -- Expected failure: See https://github.com/google/codeworld/issues/681
        let wrapped = wrappedInitial 42
        let target  = wrappedEvent (const []) (const id) (const id) (TimePassing 0) wrapped
        assertBool "" $ not $ identical wrapped target
    ]
