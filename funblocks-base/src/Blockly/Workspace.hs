{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module Blockly.Workspace ( Workspace(..), setWorkspace, workspaceToCode)
  where

import GHCJS.Types
import Data.JSString (pack, unpack)
import GHCJS.Foreign
import GHCJS.Marshal

newtype Workspace = Workspace JSVal

instance IsJSVal Workspace

instance ToJSVal Workspace where
  toJSVal (Workspace v) = return v

instance FromJSVal Workspace where
  fromJSVal v = return $ Just $ Workspace v


setWorkspace :: String -> String -> IO Workspace
setWorkspace canvasId toolboxId =  js_blocklyInject (pack canvasId) (pack toolboxId)

workspaceToCode :: Workspace -> IO String
workspaceToCode workspace = js_blocklyWorkspaceToCode workspace >>= return . unpack


--- FFI
foreign import javascript unsafe "Blockly.inject($1, { toolbox: document.getElementById($2), css: false})"
  js_blocklyInject :: JSString -> JSString -> IO Workspace

foreign import javascript unsafe "Blockly.FunBlocks.workspaceToCode($1)"
  js_blocklyWorkspaceToCode :: Workspace -> IO JSString

