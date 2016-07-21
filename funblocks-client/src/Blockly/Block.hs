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
                     , getOutputBlock
                     , getOutputConnection
                     , getColour
                     , getFunctionName
                     , getItemCount
                     , setColour
                     , areAllInputsConnected
                     , select
                     , addSelect
                     , addErrorSelect
                     , setWarningText
                     , disableWarningText
                     , setDisabled
                     , getInputBlock)
  where

import GHCJS.Types
import Data.JSString.Text
import GHCJS.Foreign
import GHCJS.Marshal
import Unsafe.Coerce
import Blockly.Connection
import qualified Data.Text as T

newtype Block = Block JSVal

instance IsJSVal Block

pack = textToJSString
unpack = textFromJSString

getFieldValue :: Block -> T.Text -> T.Text
getFieldValue block fieldName = unpack $ js_getFieldValue block (pack fieldName)

instance ToJSVal Block where
  toJSVal (Block v) = return v

instance FromJSVal Block where
  fromJSVal v = return $ Just $ Block v

getBlockType :: Block -> T.Text
getBlockType = unpack . js_type

getOutputBlock :: Block -> Maybe Block
getOutputBlock block = if isNull con then Nothing
                       else let block = js_outputConnectionBlock' con in 
                         if isNull block then Nothing
                         else Just $ unsafeCoerce block
  where con = js_outputConnection' block

getOutputConnection :: Block -> Maybe Connection
getOutputConnection block = if isNull con then Nothing
                      else Just $ Connection con 
  where
    con = js_outputConnection' block

blockTest :: Block -> IO ()
blockTest = js_testOutputConnection

setColour :: Block -> Int -> IO ()
setColour = js_setColour 

getColour :: Block -> Int
getColour = js_getColour 

getFunctionName :: Block -> T.Text
getFunctionName = unpack . js_getFunctionName 

getInputBlock :: Block -> T.Text -> Maybe Block
getInputBlock block name = if isNull val then Nothing
                           else Just $ Block val
  where val = js_getInputTargetBlock block (pack name)

select :: Block -> IO ()
select = js_select 

addSelect :: Block -> IO ()
addSelect = js_addSelect 

addErrorSelect :: Block -> IO ()
addErrorSelect = js_addErrorSelect 

setWarningText :: Block -> T.Text -> IO ()
setWarningText block text = js_setWarningText block (pack text)

disableWarningText :: Block ->  IO ()
disableWarningText = js_disableWarningText 

setDisabled :: Block -> Bool -> IO ()
setDisabled = js_setDisabled 

areAllInputsConnected :: Block -> Bool
areAllInputsConnected = js_allInputsConnected

getItemCount :: Block -> Int
getItemCount = js_itemCount

--- FFI

foreign import javascript unsafe "$1.itemCount_"
  js_itemCount :: Block -> Int

foreign import javascript unsafe "$1.getFieldValue($2)"
  js_getFieldValue :: Block -> JSString -> JSString

foreign import javascript unsafe "$1.type"
  js_type :: Block -> JSString

-- getConnection
foreign import javascript unsafe "$1.outputConnection"
  js_outputConnection' :: Block -> JSVal

foreign import javascript unsafe "$1.outputConnection"
  js_outputConnection :: Block -> Connection

foreign import javascript unsafe "$1.allInputsConnected()"
  js_allInputsConnected :: Block -> Bool

foreign import javascript unsafe "$1.targetBlock()"
  js_outputConnectionBlock' :: JSVal -> JSVal

foreign import javascript unsafe "$1.targetBlock()"
  js_outputConnectionBlock :: Connection -> Block

foreign import javascript unsafe "alert($1.outputConnection.targetBlock())"
  js_testOutputConnection :: Block -> IO ()

foreign import javascript unsafe "$1.getColour()"
  js_getColour :: Block -> Int

foreign import javascript unsafe "$1.setColour($2)"
  js_setColour :: Block -> Int -> IO ()

foreign import javascript unsafe "$1.setDisabled($2)"
  js_setDisabled :: Block -> Bool -> IO ()

foreign import javascript unsafe "$1.select()"
  js_select :: Block -> IO ()

foreign import javascript unsafe "$1.addSelect()"
  js_addSelect :: Block -> IO ()

foreign import javascript unsafe "$1.addErrorSelect()"
  js_addErrorSelect :: Block -> IO ()

foreign import javascript unsafe "$1.getFunctionName()"
  js_getFunctionName :: Block -> JSString

foreign import javascript unsafe "$1.setWarningText($2)"
  js_setWarningText :: Block -> JSString -> IO ()

foreign import javascript unsafe "$1.setWarningText(null)"
  js_disableWarningText :: Block -> IO ()


-- fetches the block associated with the input name or else null
foreign import javascript unsafe "$1.getInputTargetBlock($2)"
  js_getInputTargetBlock :: Block -> JSString -> JSVal
