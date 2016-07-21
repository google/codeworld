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
          | NoType -- For top level blocks

instance Show Type where
  show Number = "Number"
  show Picture = "Picture"
  show (Custom (User name adt)) = name
  show (Poly c) = c:""
  show (Str) = "Text"
  show (Bool) = "Bool"
  show (Col) = "Color"
  show (Arrow tps) = intercalate " -> " $ map show tps



data FieldType = LeftField | RightField | CentreField

data Input = Value T.Text [Field] Type
            | Statement T.Text [Field] Type
            | Dummy [Field]

data Field = Text T.Text
            | TextE T.Text -- Emphasized Text, for titles
            | TextInput T.Text T.Text -- displayname, value
            
data Connection = TopCon | BotCon | TopBotCon | LeftCon
newtype Inline = Inline Bool

-- Name functionName inputs connectiontype color outputType tooltip
data DesignBlock = DesignBlock T.Text T.Text [Input] Inline Color Type Tooltip


newtype Color = Color Int
newtype Tooltip = Tooltip T.Text

fieldCode :: FieldInput -> Field -> IO FieldInput
fieldCode field (Text str) = js_appendTextField field (pack str)
fieldCode field (TextE str) = js_appendTextFieldEmph field (pack str)
fieldCode field (TextInput text name) = js_appendTextInputField field (pack text) (pack name)

inputCode :: Block -> [TypeVar] -> Input -> IO ()
inputCode block _ (Dummy fields) = do 
  fieldInput <- js_appendDummyInput block  
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  return ()

inputCode block pvars (Value name fields (Poly polyChar) ) = do
    fieldInput <- js_appendValueInput block (pack name)
    foldr (\ field fi -> do
        fi_ <- fi
        fieldCode fi_ field) (return fieldInput) fields
    js_setTypeExprPoly fieldInput (pvars !! polyIndex)
    return ()
  where
    table = zip (['a'..'z'] ++ ['A'..'Z']) [0..] -- Compatibility hack
    polyIndex = fromJust $ lookup polyChar table

-- TODO, change function later to express correct type
-- For now we just show the string expression
-- Later the TypeExpr will be properly set
inputCode block _ (Value name fields (type_) ) = do
  fieldInput <- js_appendValueInput block (pack name)
  js_setCheck fieldInput (pack $ T.pack $ show type_)
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  js_setTypeExprConc fieldInput (pack $ T.pack $ show type_) -- TODO, this is a compatibility hack
  return ()


-- Gets the highest number of polymorphic inputs
maxPolyCount :: [Input] -> Int
maxPolyCount inputs = case maxInp of
                        Value _ _ (Poly c) -> (ind c)+1
                        _ -> 0
  where 
    maxInp = maximumBy (comparing auxComp) inputs 
    auxComp (Value _ _ (Poly ind)) = ind
    auxComp _ = 'a'
    table = zip (['a'..'z'] ++ ['A'..'Z']) [0..] -- Compatibility hack
    ind c = fromJust $ lookup c table

-- set block
setBlockType :: DesignBlock -> IO ()
setBlockType (DesignBlock name funName inputs (Inline inline) (Color color) type_ (Tooltip tooltip) ) = do
  cb <- syncCallback1 ContinueAsync  (\this -> do 
                                 let block = Block this
                                 js_setFunctionName block (pack funName)
                                 js_setColor block color
                                 -- may error out if no type is set
                                 tvars <- replicateM (maxPolyCount inputs) js_getUnusedTypeVar
                                 mapM_ (inputCode block tvars) inputs
                                 js_enableOutput block
                                 case type_ of
                                     Poly polyChar -> let 
                                                      table = zip (['a'..'z'] ++ ['A'..'Z']) [0..] -- Compatibility hack
                                                      ind = fromJust $ lookup polyChar table
                                                  in
                                        js_setOutputTypePoly block (tvars !! ind)
                                     NoType -> js_disableOutput block 
                                     tp -> js_setOutputTypeConc block (pack $ T.pack $ show tp)
                                 when inline $ js_setInputsInline block True
                                    -- else return ()
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
  js_getUnusedTypeVar :: IO TypeVar

foreign import javascript unsafe "$1.setTypeExpr($2)"
  js_setTypeExprPoly :: FieldInput -> TypeVar -> IO ()

foreign import javascript unsafe "$1.setTypeExpr(new Blockly.TypeExpr($2))"
  js_setTypeExprConc :: FieldInput -> JSString -> IO ()

foreign import javascript unsafe "$1.setInputsInline($2)"
  js_setInputsInline :: Block -> Bool -> IO ()
