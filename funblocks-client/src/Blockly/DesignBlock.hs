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
import Data.JSString.Text
import GHCJS.Marshal
import GHCJS.Foreign hiding (Number, String)
import GHCJS.Foreign.Callback
import Control.Monad
import Data.Ord (comparing)
import Data.List (maximumBy)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.List (intercalate)
import qualified Blockly.TypeExpr as TE
import qualified JavaScript.Array as JA

pack = textToJSString
unpack = textFromJSString

-- Low level bindings to construction of various different type of Blockly
-- blocks


data ADT = Product String [Type]
          | Sum [ADT]

data User = User String ADT

instance Show User where
  show (User name adt) = name

data Type = Arrow [Type]
          | Number
          | Str -- Actually Text
          | Bool
          | Picture
          | Col -- Actually Color
          | List Type -- Have to define kinded types
          | Custom User
          | Poly Char
          | Top -- For top level blocks
          | Comment

instance Show Type where
  show Number = "Number"
  show Picture = "Picture"
  show (Custom (User name adt)) = name
  show (Poly c) = c:""
  show (Str) = "Text"
  show (Bool) = "Bool"
  show (Col) = "Color"
  show (Comment) = ""
  show (Top) = ""
  show (Arrow tps) = intercalate " -> " $ map show tps



data FieldType = LeftField | RightField | CentreField

data Input = Value T.Text [Field] 
--            | Statement T.Text [Field] Type
            | Dummy [Field]

data Field = Text T.Text
            | TextE T.Text -- Emphasized Text, for titles
            | TextInput T.Text T.Text -- displayname, value
            
data Connection = TopCon | BotCon | TopBotCon | LeftCon
newtype Inline = Inline Bool

-- Name functionName inputs connectiontype color outputType tooltip
data DesignBlock = DesignBlock T.Text T.Text [Input] Inline Color [Type] Tooltip


newtype Color = Color Int
newtype Tooltip = Tooltip T.Text

fieldCode :: FieldInput -> Field -> IO FieldInput
fieldCode field (Text str) = js_appendTextField field (pack str)
fieldCode field (TextE str) = js_appendTextFieldEmph field (pack str)
fieldCode field (TextInput text name) = js_appendTextInputField field (pack text) (pack name)

inputCode :: Block -> Input -> IO ()
inputCode block (Dummy fields) = do 
  fieldInput <- js_appendDummyInput block  
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  return ()

inputCode block (Value name fields) = do
  fieldInput <- js_appendValueInput block (pack name)
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  return ()


typeToTypeExpr :: Type -> TE.TypeExpr
typeToTypeExpr (Poly a) = TE.createTypeExpr (T.pack $ "_POLY_" ++ [a]) []
typeToTypeExpr t = TE.createTypeExpr (T.pack $ show t) [] -- Currently still a hack


-- set block
setBlockType :: DesignBlock -> IO ()
setBlockType (DesignBlock name funName inputs (Inline inline) (Color color) arrows (Tooltip tooltip) ) = do
  cb <- syncCallback1 ContinueAsync  (\this -> do 
                                 let block = Block this
                                 js_setFunctionName block (pack funName)
                                 js_setColor block color
                                 mapM_ (inputCode block) inputs
                                 case last arrows of
                                   Top -> js_disableOutput block
                                   Comment -> js_disableOutput block
                                   _ -> js_enableOutput block
                                 case last arrows of
                                   Comment -> return ()
                                   Top -> setArrows block $ take (length arrows -1) arrows
                                   _ -> setArrows block arrows
                                 when inline $ js_setInputsInline block True
                                 return ()
                                 )
  js_setGenFunction (pack name) cb


newtype Block = Block JSVal
newtype FieldInput = FieldInput JSVal

setArrows :: Block -> [Type] -> IO ()
setArrows block tps = js_setArrows block typeExprs
  where
    typeExprs = TE.toJSArray $ map typeToTypeExpr tps

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
  js_setOutputTypePoly :: Block -> TE.TypeExpr -> IO ()

foreign import javascript unsafe "$1.setTooltip($2)"
  js_setTooltip :: Block -> JSString -> IO ()

foreign import javascript unsafe "$1.appendDummyInput()"
  js_appendDummyInput :: Block -> IO FieldInput

foreign import javascript unsafe "$1.arrows = $2"
  js_setArrows :: Block -> JA.JSArray -> IO ()

foreign import javascript unsafe "$1.appendValueInput($2)"
  js_appendValueInput :: Block -> JSString -> IO FieldInput

foreign import javascript unsafe "$1.appendField($2)"
  js_appendTextField :: FieldInput -> JSString -> IO FieldInput

foreign import javascript unsafe "$1.appendField(new Blockly.FieldLabel($2, 'blocklyTextEmph'))"
  js_appendTextFieldEmph :: FieldInput -> JSString -> IO FieldInput

foreign import javascript unsafe "$1.appendField(new Blockly.FieldTextInput($2), $3)"
  js_appendTextInputField :: FieldInput -> JSString -> JSString -> IO FieldInput
  -- field, text of field, name ref
  --
foreign import javascript unsafe "$1.setCheck($2)"
  js_setCheck :: FieldInput -> JSString -> IO ()

foreign import javascript unsafe "$1.functionName = $2"
  js_setFunctionName :: Block -> JSString -> IO ()

foreign import javascript unsafe "Blockly.TypeVar.getUnusedTypeVar()"
  js_getUnusedTypeVar :: IO TE.TypeExpr

foreign import javascript unsafe "$1.setTypeExpr($2)"
  js_setTypeExprPoly :: FieldInput -> TE.TypeExpr -> IO ()

foreign import javascript unsafe "$1.setTypeExpr(new Blockly.TypeExpr($2))"
  js_setTypeExprConc :: FieldInput -> JSString -> IO ()

foreign import javascript unsafe "$1.setInputsInline($2)"
  js_setInputsInline :: Block -> Bool -> IO ()
