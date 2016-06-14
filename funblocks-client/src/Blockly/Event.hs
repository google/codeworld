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

module Blockly.Event ( Event(..)
                      ,getType
                      ,getWorkspaceId
                      ,getBlockId
                      ,getIds)
  where

import GHCJS.Types
import Data.JSString (pack, unpack)
import GHCJS.Foreign
import GHCJS.Marshal
import Blockly.General

newtype Event = Event JSVal

-- data Event = Event
--
--           | CreateEvent
--           | DeleteEvent
--           | ChangeEvent
--           | MoveEvent
--           | UIEvent
--
-- newtype CreateEvent = CreateEvent JSVal
-- newtype DeleteEvent = DeleteEvent JSVal
-- newtype ChangeEvent = ChangeEvent JSVal
-- newtype MoveEvent = MoveEvent JSVal
-- newtype UIEvent = UIEvent JSVal

getType :: Event -> UUID
getType event = UUID $ unpack $ js_type event

getWorkspaceId :: Event -> UUID
getWorkspaceId event = UUID $ unpack $ js_type event

getBlockId :: Event -> UUID
getBlockId event = UUID $ unpack $ js_type event

getGroup :: Event -> UUID
getGroup event = UUID $ unpack $ js_type event

getIds :: Event -> UUID
getIds event = UUID $ unpack $ js_type event

--- FFI
--
-- One of Blockly.Events.CREATE, Blockly.Events.DELETE, Blockly.Events.CHANGE,
-- Blockly.Events.MOVE, Blockly.Events.UI.
foreign import javascript unsafe "$1.type"
  js_type :: Event -> JSString

-- UUID of workspace
foreign import javascript unsafe "$1.workspaceId"
  js_workspaceId :: Event -> JSString

-- UUID of block. 
foreign import javascript unsafe "$1.blockId"
  js_blockId :: Event -> JSString

-- UUID of group
foreign import javascript unsafe "$1.group"
  js_group :: Event -> JSString

-- UUIDs of affected blocks
foreign import javascript unsafe "$1.ids"
  js_ids :: Event -> [JSString]

