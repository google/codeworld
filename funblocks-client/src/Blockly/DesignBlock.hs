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

module Blockly.DesignBlock (Type(..)
                          ,FieldType(..)
                          ,Input(..)
                          ,Field(..)
                          ,Inline(..)
                          ,Connection(..)
                          ,DesignBlock(..)
                          ,Color(..)
                          ,Tooltip(..)
                          ,setBlockType)
  where

import GHCJS.Types
import Data.JSString (pack, unpack)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import Control.Monad

-- Low level bindings to construction of various different type of Blockly
-- blocks

data Type = Type String
          | Poly Int
          | NoType

data FieldType = LeftField | RightField | CentreField

data Input = Value String FieldType [Field] Type
            | Statement String [Field] Type
            | Dummy [Field]

data Field = Text String
            | TextInput String String -- displayname, value
            
data Connection = TopCon | BotCon | TopBotCon | LeftCon
newtype Inline = Inline Bool

-- Name inputs connectiontype color outputType tooltip
data DesignBlock = DesignBlock String [Input] Inline Color Type Tooltip


newtype Color = Color Int
newtype Tooltip = Tooltip String

fieldCode :: FieldInput -> Field -> IO FieldInput
fieldCode field (Text str) = js_appendTextField field (pack str)
fieldCode field (TextInput text name) = js_appendTextInputField field (pack text) (pack name)

inputCode :: Block -> [TypeVar] -> Input -> IO ()
inputCode block _ (Dummy fields) = do 
  fieldInput <- js_appendDummyInput block  
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  return ()

inputCode block _ (Value name fieldType fields (Type type_) ) = do
  fieldInput <- js_appendValueInput block (pack name)
  js_setCheck fieldInput (pack type_)
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  js_setTypeExprConc fieldInput (pack type_)
  return ()

inputCode block pvars (Value name fieldType fields (Poly polyIndex) ) = do
  fieldInput <- js_appendValueInput block (pack name)
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  js_setTypeExprPoly fieldInput (pvars !! polyIndex)
  return ()



 
-- set block
setBlockType :: DesignBlock -> IO ()
setBlockType (DesignBlock name inputs (Inline inline) (Color color) type_ (Tooltip tooltip) ) = do
  cb <- syncCallback1 ContinueAsync  (\this -> do 
                                 let block = Block this 
                                 js_setColor block color
                                 -- may error out if no type is set
                                 tvars <- replicateM (length inputs) js_getUnusedTypeVar
                                 mapM_ (inputCode block tvars) inputs
                                 js_enableOutput block
                                 case type_ of
                                     Type tp -> js_setOutputTypeConc block (pack tp)
                                     Poly ind -> js_setOutputTypePoly block (tvars !! ind)
                                     _ -> js_disableOutput block 
                                 case inline of
                                    True -> js_setInputsInline block True
                                    _ -> return ()
                                 return ()
                                 )
  js_setGenFunction (pack name) cb


newtype Block = Block JSVal
newtype FieldInput = FieldInput JSVal
newtype TypeVar = TypeVar JSVal

foreign import javascript unsafe "Blockly.Blocks[$1] = { init: function() { $2(this); }}"
  js_setGenFunction :: JSString -> Callback a -> IO ()

foreign import javascript unsafe "$1.setColour($2)"
  js_setColor :: Block -> Int -> IO ()

foreign import javascript unsafe "$1.setOutput(true)"
  js_enableOutput :: Block -> IO ()

foreign import javascript unsafe "$1.setOutput(false)"
  js_disableOutput:: Block -> IO ()


foreign import javascript unsafe "$1.setOutputTypeExpr(new Blockly.TypeExpr($2))"
  js_setOutputTypeConc :: Block -> JSString -> IO ()

foreign import javascript unsafe "$1.setOutputTypeExpr($2)"
  js_setOutputTypePoly :: Block -> TypeVar -> IO ()

foreign import javascript unsafe "$1.setTooltip($2)"
  js_setTooltip :: Block -> JSString -> IO ()

foreign import javascript unsafe "$1.appendDummyInput()"
  js_appendDummyInput :: Block -> IO FieldInput

foreign import javascript unsafe "$1.appendValueInput($2)"
  js_appendValueInput :: Block -> JSString -> IO FieldInput

foreign import javascript unsafe "$1.appendField($2)"
  js_appendTextField :: FieldInput -> JSString -> IO FieldInput

foreign import javascript unsafe "$1.appendField(new Blockly.FieldTextInput($2), $3)"
  js_appendTextInputField :: FieldInput -> JSString -> JSString -> IO FieldInput
  -- field, text of field, name ref
  --
foreign import javascript unsafe "$1.setCheck($2)"
  js_setCheck :: FieldInput -> JSString -> IO ()

foreign import javascript unsafe "Blockly.TypeVar.getUnusedTypeVar()"
  js_getUnusedTypeVar :: IO TypeVar

foreign import javascript unsafe "$1.setTypeExpr($2)"
  js_setTypeExprPoly :: FieldInput -> TypeVar -> IO ()

foreign import javascript unsafe "$1.setTypeExpr(new Blockly.TypeExpr($2))"
  js_setTypeExprConc :: FieldInput -> JSString -> IO ()

foreign import javascript unsafe "$1.setInputsInline($2)"
  js_setInputsInline :: Block -> Bool -> IO ()
