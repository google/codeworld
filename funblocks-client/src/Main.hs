-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Main (
    main
) where

import GHCJS.DOM
       (currentDocument, )
import GHCJS.DOM.Document (getBody, getElementById, Document(..))
import GHCJS.DOM.Element (setInnerHTML, click, Element)
import GHCJS.DOM.EventM (on )
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import Data.JSString.Text
import qualified Data.Text as T
import Control.Monad.Trans (liftIO)
import Blockly.Workspace hiding (workspaceToCode)
import Blocks.Parser
import Blocks.CodeGen
import Blocks.Types
import Blockly.Event
import Blockly.General
import Blockly.Block 

pack = textToJSString
unpack = textFromJSString

setErrorMessage msg = do
  Just doc <- liftIO currentDocument
  Just msgEl <- getElementById doc "message"
  setInnerHTML msgEl $ Just msg

programBlocks :: [T.Text]
programBlocks = map T.pack ["cwDrawingOf","cwAnimationOf", "cwSimulationOf"]

btnStopClick = do 
  liftIO js_stop

btnRunClick ws = do
  Just doc <- liftIO currentDocument
  blocks <- liftIO $ getTopBlocks ws
  if not $ containsProgramBlock blocks 
    then setErrorMessage "Error: No Program on Workspace"
    else do
      (code,errors) <- liftIO $ workspaceToCode ws
      case errors of
        ((Error msg block):es) -> do 
                                    setErrorMessage msg
                                    liftIO $ setWarningText block msg
                                    liftIO $ addErrorSelect block
                                    liftIO $ js_removeErrorsDelay
                                    liftIO $ js_stop

        [] -> do
          liftIO $ js_updateEditor (pack code)
          liftIO $ js_cwcompile (pack code)
  return ()
  where
    containsProgramBlock = any (\b -> getBlockType b `elem` programBlocks) 

hookEvent elementName evType func = do
  Just doc <- currentDocument
  Just ele <- getElementById doc elementName
  on ele evType func

main = do 
      Just doc <- currentDocument 
      Just body <- getBody doc
      workspace <- liftIO $ setWorkspace "blocklyDiv" "toolbox"
      liftIO $ disableOrphans workspace -- Disable disconnected non-top level blocks
      hookEvent "btnRun" click (btnRunClick workspace)
      hookEvent "btnStop" click btnStopClick
      liftIO setBlockTypes -- assign layout and types of Blockly blocks
      liftIO $ addChangeListener workspace (onChange workspace)
      liftIO $ js_showEast
      liftIO $ js_openEast
      return ()

-- Update code in real time
onChange ws event = do 
                      (code, errs) <- workspaceToCode ws
                      js_updateEditor (pack code)

-- FFI

-- call blockworld.js compile
foreign import javascript unsafe "compile($1)"
  js_cwcompile :: JSString -> IO ()

-- call blockworld.js run
-- run (xmlHash, codeHash, msg, error)
foreign import javascript unsafe "run()"
  js_cwrun :: JSString -> JSString -> JSString -> Bool -> IO ()

-- call blockworld.js updateUI
foreign import javascript unsafe "updateUI()"
  js_updateUI :: IO ()

-- funnily enough, If I'm calling run "" "" "" False I get errors
foreign import javascript unsafe "run('','','',false)"
  js_stop :: IO ()

foreign import javascript unsafe "updateEditor($1)"
  js_updateEditor :: JSString -> IO ()

foreign import javascript unsafe "setTimeout(removeErrors,10000)"
  js_removeErrorsDelay :: IO ()

foreign import javascript unsafe "window.mainLayout.show('east')"
  js_showEast :: IO ()

foreign import javascript unsafe "window.mainLayout.open('east')"
  js_openEast :: IO ()
