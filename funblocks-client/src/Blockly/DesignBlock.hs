{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

{-
  Copyright 2019 The CodeWorld Authors. All Rights Reserved.

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
                          ,BlockType(..)
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
import GHCJS.Foreign hiding (Number, String, Function)
import GHCJS.Foreign.Callback
import Data.Monoid
import Control.Monad
import Data.Ord (comparing)
import Data.List (maximumBy)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.List (intercalate)
import qualified Blockly.TypeExpr as TE
import qualified JavaScript.Array as JA
import Blockly.Block as B

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
          | Truth
          | Picture
          | Col -- Actually Color
          | List Type -- Have to define kinded types
          | Custom User
          | Poly T.Text
          | Program -- For top level blocks
          | Comment

instance Show Type where
  show Number = "Number"
  show Picture = "Picture"
  show (Custom (User name adt)) = name
  show (Poly c) = T.unpack c
  show (Str) = "Text"
  show (Truth) = "Truth"
  show (Col) = "Color"
  show (Comment) = ""
  show (Program) = "Program"
  show (Arrow tps) = intercalate " -> " $ map show tps
  show (List tp) = "[" ++ show tp ++ "]"


data FieldType = LeftField | RightField | CentreField

data Input = Value T.Text [Field] 
--            | Statement T.Text [Field] Type
            | Dummy [Field]

data Field = Text T.Text
            | TextE T.Text -- Emphasized Text, for titles
            | TextInput T.Text T.Text -- displayname, value
            | FieldImage T.Text Int Int -- src, width, height
            
data Connection = TopCon | BotCon | TopBotCon | LeftCon
newtype Inline = Inline Bool

-- Name functionName inputs connectiontype color outputType tooltip
-- name funcName
data BlockType = Literal T.Text
               | Function T.Text [Type]
               | Top T.Text [Type]
               | None -- do nothing !
-- DesignBlock name type inputs isInline Color Tooltip
data DesignBlock = DesignBlock T.Text BlockType [Input] Inline Color Tooltip


newtype Color = Color Int
newtype Tooltip = Tooltip T.Text

fieldCode :: FieldInput -> Field -> IO FieldInput
fieldCode field (Text str) = js_appendTextField field (pack str)
fieldCode field (TextE str) = js_appendTextFieldEmph field (pack str)
fieldCode field (TextInput text name) = js_appendTextInputField field (pack text) (pack name)
fieldCode field (FieldImage src width height) = js_appendFieldImage field (pack src) width height

inputCode :: Bool -> Block -> Input -> IO ()
inputCode rightAlign block (Dummy fields) = do 
  fieldInput <- js_appendDummyInput block
  when rightAlign $ js_setAlignRight fieldInput
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  return ()
inputCode rightAlign block (Value name fields) = do
  fieldInput <- js_appendValueInput block (pack name)
  when rightAlign $ js_setAlignRight fieldInput
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  return ()

typeToTypeExpr :: Type -> TE.Type_
typeToTypeExpr (Poly a) = TE.createType $ TE.TypeVar a
typeToTypeExpr t = TE.createType $ TE.Lit (T.pack $ show t ) [] -- Currently still a hack


-- set block
setBlockType :: DesignBlock -> IO ()
setBlockType (DesignBlock name blockType inputs (Inline inline) (Color color) (Tooltip tooltip) ) = do
      cb <- syncCallback1 ContinueAsync  (\this -> do 
                                     let block = B.Block this
                                     js_setColor block color
                                     forM_ (zip inputs (False : repeat True)) $ \(inp, rightAlign) -> do
                                       inputCode rightAlign block inp
                                     case blockType of
                                       None -> js_disableOutput block
                                       Top _ _ -> js_disableOutput block
                                       _ -> js_enableOutput block
                                     assignBlockType block blockType
                                     when inline $ js_setInputsInline block True
                                     return ()
                                     )
      js_setGenFunction (pack name) cb

typeToType :: Type -> TE.Type
typeToType (Poly a) = TE.TypeVar a
typeToType (List tp) = TE.Lit (T.pack "list") [typeToType tp] 
typeToType lit = TE.Lit (T.pack $ show lit) []

assignBlockType :: Block -> BlockType -> IO ()
assignBlockType block (Literal name) = B.setAsLiteral block name
assignBlockType block (Function name tps) = do 
    js_defineFunction (pack name) (TE.fromList tp)
    B.setAsFunction block name
  where tp = map typeToType tps
assignBlockType block (Top name tps) = assignBlockType block (Function name tps)
assignBlockType _ _ = return ()
  
newtype FieldInput = FieldInput JSVal

-- setArrows :: Block -> [Type] -> IO ()
-- setArrows block tps = js_setArrows block typeExprs
--   where
--     typeExprs = TE.toJSArray $ map typeToTypeExpr tps

foreign import javascript unsafe "Blockly.Blocks[$1] = { init: function() { $2(this); }}"
  js_setGenFunction :: JSString -> Callback a -> IO ()

foreign import javascript unsafe "$1.setColour($2)"
  js_setColor :: Block -> Int -> IO ()

foreign import javascript unsafe "$1.setOutput(true)"
  js_enableOutput :: Block -> IO ()

foreign import javascript unsafe "$1.setOutput(false)"
  js_disableOutput:: Block -> IO ()

foreign import javascript unsafe "$1.setTooltip($2)"
  js_setTooltip :: Block -> JSString -> IO ()

foreign import javascript unsafe "$1.appendDummyInput()"
  js_appendDummyInput :: Block -> IO FieldInput

foreign import javascript unsafe "Blockly.TypeInf.defineFunction($1, $2)"
  js_defineFunction :: JSString -> TE.Type_ -> IO ()

foreign import javascript unsafe "$1.appendValueInput($2)"
  js_appendValueInput :: Block -> JSString -> IO FieldInput

foreign import javascript unsafe "$1.appendField($2)"
  js_appendTextField :: FieldInput -> JSString -> IO FieldInput

foreign import javascript unsafe "$1.appendField(new Blockly.FieldLabel($2, 'blocklyTextEmph'))"
  js_appendTextFieldEmph :: FieldInput -> JSString -> IO FieldInput

foreign import javascript unsafe "$1.appendField(new Blockly.FieldImage($2, $3, $4))"
  js_appendFieldImage:: FieldInput -> JSString -> Int -> Int -> IO FieldInput

foreign import javascript unsafe "$1.appendField(new Blockly.FieldTextInput($2), $3)"
  js_appendTextInputField :: FieldInput -> JSString -> JSString -> IO FieldInput

foreign import javascript unsafe "$1.setCheck($2)"
  js_setCheck :: FieldInput -> JSString -> IO ()

foreign import javascript unsafe "$1.setAlign(Blockly.ALIGN_RIGHT)"
  js_setAlignRight :: FieldInput -> IO ()

foreign import javascript unsafe "$1.setInputsInline($2)"
  js_setInputsInline :: Block -> Bool -> IO ()
