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

checkWrappedIdentity :: (Wrapped a -> Wrapped a) -> Wrapped a -> Assertion
checkWrappedIdentity f x = assertBool "identity" (identical x (f x))

tests :: Test
tests = testGroup "Driver"
    [ testCase "toState preserves identity" $
          checkWrappedIdentity (toState id) (wrappedInitial 42)
    , testCase "wrappedStep preserves identity" $
          checkWrappedIdentity (wrappedStep (const id) 1) (wrappedInitial 42)
    , testCase "wrapping of shared identity is shared (events)" $
          checkWrappedIdentity (wrappedEvent (const [])
                                             (const id)
                                             (const id)
                                             (TimePassing 1))
                               (wrappedInitial 42)
    ]
