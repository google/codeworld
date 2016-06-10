{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module Blockly.DesignBlock (Type(..)
                          ,FieldType(..)
                          ,Input(..)
                          ,Field(..)
                          ,Connection(..)
                          ,DesignBlock(..)
                          ,Color
                          ,setBlockType)
  where

import GHCJS.Types
import Data.JSString (pack, unpack)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Foreign.Callback



data Type = Type String
          | NoType

data FieldType = LeftField | RightField | CentreField

data Input = Value String FieldType [Field] Type
            | Statement String [Field] Type
            | Dummy [Field]

data Field = Text String
            | TextInput String String -- displayname, value
            
data Connection = TopCon | BotCon | TopBotCon | LeftCon

-- Name inputs connectiontype color outputType tooltip
data DesignBlock = DesignBlock String [Input] Connection Color Type String

type Color = Int

fieldCode :: FieldInput -> Field -> IO FieldInput
fieldCode field (Text str) = js_appendTextField field (pack str)
fieldCode field (TextInput text name) = js_appendTextInputField field (pack text) (pack name)

inputCode :: Block -> Input -> IO ()
inputCode block (Dummy fields) = do 
  fieldInput <- js_appendDummyInput block  
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  return ()

inputCode block (Value name fieldType fields (Type type_) ) = do
  fieldInput <- js_appendValueInput block (pack name)
  js_setCheck fieldInput (pack type_)
  foldr (\ field fi -> do
      fi_ <- fi
      fieldCode fi_ field) (return fieldInput) fields
  return ()

 
-- set block
setBlockType :: DesignBlock -> IO ()
setBlockType (DesignBlock name inputs connection color type_ tooltip) = do
  cb <- syncCallback1 ContinueAsync  (\this -> do 
                                 let block = Block this 
                                 js_setColor block color
                                 -- may error out if no type is set
                                 case type_ of
                                     Type tp -> js_setOutput block True (pack tp)
                                     NoType -> return ()
                                 mapM_ (inputCode block) inputs
                                 return ()
                                 )
  js_setGenFunction (pack name) cb


newtype Block = Block JSVal
newtype FieldInput = FieldInput JSVal

foreign import javascript unsafe "Blockly.Blocks[$1] = { init: function() { $2(this); }}"
  js_setGenFunction :: JSString -> Callback a -> IO ()

foreign import javascript unsafe "$1.setColour($2)"
  js_setColor :: Block -> Int -> IO ()

foreign import javascript unsafe "$1.setOutput($2, $3)"
  js_setOutput :: Block -> Bool -> JSString -> IO ()

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


