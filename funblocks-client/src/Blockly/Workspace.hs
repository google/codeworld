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

module Blockly.Workspace ( Workspace(..)
                          ,setWorkspace
                          ,workspaceToCode
                          ,isTopBlock
                          ,getById
                          ,getBlockById
                          )
  where

import GHCJS.Types
import Data.JSString (pack, unpack)
import GHCJS.Foreign
import GHCJS.Marshal
import Unsafe.Coerce
import Blockly.General
import Blockly.Block (Block)

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

getById :: UUID -> Workspace
getById (UUID uuidStr) = js_getById (pack uuidStr)

getBlockById :: Workspace -> UUID -> Maybe Block
getBlockById workspace (UUID uuidstr) = if isNull val then Nothing
                                        else Just $ unsafeCoerce val
  where val = js_getBlockById workspace (pack uuidstr)

isTopBlock :: Workspace -> Block -> Bool
isTopBlock = js_isTopBlock

--- FFI

-- TODO Maybe use a list of properties ?
foreign import javascript unsafe "Blockly.inject($1, { toolbox: document.getElementById($2), css: false, disable: false, comments: false, zoom:{wheel:true, controls: true}})"
  js_blocklyInject :: JSString -> JSString -> IO Workspace

foreign import javascript unsafe "Blockly.FunBlocks.workspaceToCode($1)"
  js_blocklyWorkspaceToCode :: Workspace -> IO JSString

foreign import javascript unsafe "$1.isTopBlock($2)"
  js_isTopBlock :: Workspace -> Block -> Bool 

foreign import javascript unsafe "Blockly.Workspace.getById($1)"
  js_getById :: JSString -> Workspace

foreign import javascript unsafe "$1.getBlockById($2)"
  js_getBlockById :: Workspace -> JSString -> JSVal

