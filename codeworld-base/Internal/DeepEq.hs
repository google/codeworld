{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE PackageImports           #-}

{-
    The really hairy bits here are written by luite.  Note that this
    code is very dependent on the internals of GHCJS, so complain if
    this is broken.
-}

{- If you want to do these things you are a bad person and you should feel bad -}

module Internal.DeepEq where

import        Control.Exception (evaluate)
import        Control.Monad
import "base" Prelude
import        System.IO.Unsafe
import        Unsafe.Coerce

#ifdef ghcjs_HOST_OS

import        GHCJS.Types
import        GHCJS.Foreign

-- traverse the object and get the thunks out of it
foreign import javascript unsafe
  "getThunks($1)" js_getThunks :: Int -> IO (JSArray Int)

foreign import javascript unsafe
  "deepEq($1,$2)" js_deepEq :: Int -> Int -> IO Bool

data JSRefD a = JSRefD a

evaluateFully :: a -> IO a
evaluateFully x = do
  x'  <- evaluate x
  ths <- js_getThunks (unsafeCoerce x')
  when (not $ isNull ths) $ do
    ths' <- fromArray ths
    forM_ ths' evalElem
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
