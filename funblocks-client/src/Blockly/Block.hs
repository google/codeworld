{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

{-
  Copyright 2016 The CodeWorld Authors. All Rights Reserved.

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

module Blockly.Block ( Block(..)
                     , getFieldValue
                     , getBlockType
                     , blockTest
                     , getOutputBlock
                     , getColour
                     , setColour)
  where

import GHCJS.Types
import Data.JSString (pack, unpack)
import GHCJS.Foreign
import GHCJS.Marshal
import Unsafe.Coerce

newtype Block = Block JSVal

instance IsJSVal Block

getFieldValue :: Block -> String -> String
getFieldValue block fieldName = unpack $ js_getFieldValue block (pack fieldName)

instance ToJSVal Block where
  toJSVal (Block v) = return v

instance FromJSVal Block where
  fromJSVal v = return $ Just $ Block v

getBlockType :: Block -> String
getBlockType = unpack . js_type

getOutputBlock :: Block -> Maybe Block
getOutputBlock block = if isNull con then Nothing
                       else let block = js_outputConnectionBlock con in 
                         if isNull block then Nothing
                         else Just $ unsafeCoerce block
  where con = js_outputConnection block

blockTest :: Block -> IO ()
blockTest = js_testOutputConnection

setColour :: Block -> Int -> IO ()
setColour = js_setColour 

getColour :: Block -> Int
getColour = js_getColour 

--- FFI

foreign import javascript unsafe "$1.getFieldValue($2)"
  js_getFieldValue :: Block -> JSString -> JSString

foreign import javascript unsafe "$1.type"
  js_type :: Block -> JSString

-- getConnection
foreign import javascript unsafe "$1.outputConnection"
  js_outputConnection :: Block -> JSVal

foreign import javascript unsafe "$1.targetBlock()"
  js_outputConnectionBlock :: JSVal -> JSVal

foreign import javascript unsafe "alert($1.outputConnection.targetBlock())"
  js_testOutputConnection :: Block -> IO ()

foreign import javascript unsafe "$1.getColour()"
  js_getColour :: Block -> Int

foreign import javascript unsafe "$1.setColour($2)"
  js_setColour :: Block -> Int -> IO ()


