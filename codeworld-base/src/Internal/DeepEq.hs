{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE PackageImports           #-}

{-
  Copyright 2015 Google Inc. All rights reserved.

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

{-
    Thanks to Luite Stegeman, whose code this is heavily based on.

    Note that this code is very dependent on the internals of GHCJS, so
    complain if this is broken.  It's not unlikely.
-}

{- If you want to do these things you are a bad person and you should feel bad -}

module Internal.DeepEq where

import        Control.Exception (evaluate)
import        Control.Monad
import "base" Prelude
import        System.IO.Unsafe
import        Unsafe.Coerce

#ifdef ghcjs_HOST_OS

import        GHCJS.Foreign
import        GHCJS.Types
import        JavaScript.Array

-- traverse the object and get the thunks out of it
foreign import javascript unsafe "cw$getThunks($1)"
  js_getThunks :: Int -> IO JSArray

foreign import javascript unsafe "cw$deepEq($1, $2)"
  js_deepEq :: Int -> Int -> IO Bool

data JSRefD a = JSRefD a

evaluateFully :: a -> IO a
evaluateFully x = do
  x'  <- evaluate x
  ths <- js_getThunks (unsafeCoerce x')
  when (not $ isNull $ unsafeCoerce ths) $ forM_ (toList ths) evalElem
  return x'
  where
    evalElem :: JSRef Int -> IO ()
    evalElem y =
      let (JSRefD o) = unsafeCoerce y in void (evaluateFully o)

deepEq :: a -> a -> Bool
deepEq x y = unsafePerformIO $ do
  x' <- evaluateFully x
  y' <- evaluateFully y
  js_deepEq (unsafeCoerce x') (unsafeCoerce y')

#else

evaluateFully :: a -> IO a
evaluateFully x = error "Only available with GHCJS"

deepEq :: a -> a -> Bool
deepEq x y = error "Only available with GHCJS"

#endif
