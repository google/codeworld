{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module Blockly.Block ( Block(..)
                     , getFieldValue)
  where

import GHCJS.Types
import Data.JSString (pack, unpack)
import GHCJS.Foreign
import GHCJS.Marshal

newtype Block = Block JSVal

instance IsJSVal Block

getFieldValue :: Block -> String -> String
getFieldValue block fieldName = unpack $ js_getFieldValue block (pack fieldName)

instance ToJSVal Block where
  toJSVal (Block v) = return v

instance FromJSVal Block where
  fromJSVal v = return $ Just $ Block v

getType :: Block -> String
getType = unpack . js_type

--- FFI

foreign import javascript unsafe "$1.getFieldValue($2)"
  js_getFieldValue :: Block -> JSString -> JSString

foreign import javascript unsafe "$1.type"
  js_type :: Block -> JSString
