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

