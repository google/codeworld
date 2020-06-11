{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

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

import CodeWorld
import CodeWorld.EntryPoints
import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

main :: IO ()
main = defaultMain [wrapperIdentityTests]

checkWrappedIdentity :: (Wrapped a -> Wrapped a) -> Wrapped a -> Assertion
checkWrappedIdentity f x = assertBool "identity" (identical x (f x))

wrapperIdentityTests :: Test
wrapperIdentityTests = testGroup "wrapperIdentityTests"
    [ testCase "toState preserves identity" $
          checkWrappedIdentity (toState id) (wrappedInitial (42 :: Int))
    , testCase "wrappedStep preserves identity" $
          checkWrappedIdentity (wrappedStep (const id) 1) (wrappedInitial (42 :: Int))
    , testCase "wrapping of shared identity is shared (events)" $
          checkWrappedIdentity (wrappedEvent (const [])
                                             (const id)
                                             (const id)
                                             (TimePassing 1))
                               (wrappedInitial (42 :: Int))
    ]
