-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

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

module Main (
    main
) where

#ifdef ghcjs_HOST_OS
#else
import qualified Prelude
#endif
import GHCJS.DOM
       (currentDocument, )
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.GlobalEventHandlers
import GHCJS.DOM.Document (getBody, Document(..))
import GHCJS.DOM.Element (setInnerHTML, Element)
import GHCJS.DOM.HTMLButtonElement
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Types
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import qualified GHCJS.Foreign.Callback as Callback
import GHCJS.Marshal
import Data.JSString.Text
import qualified Data.JSString as JStr
import qualified Data.Text as T
import Control.Monad.Trans (liftIO, lift)
import Blockly.Workspace hiding (workspaceToCode)
import Blocks.Parser
import Blocks.CodeGen
import Blocks.Types
import Blockly.Event
import Blockly.General
import Blockly.Block 
import Data.Monoid 
import Control.Monad

pack = textToJSString
unpack = textFromJSString

setErrorMessage msg = do
  Just doc <- liftIO currentDocument
  -- Just msgEl <- getElementById doc "message"
  liftIO $ putStrLn msg
  liftIO $ js_stopErr (JStr.pack msg)
  -- setInnerHTML msgEl $ Just msg

programBlocks :: [T.Text]
programBlocks = map T.pack ["cwActivityOf", "cwDrawingOf","cwAnimationOf", "cwSimulationOf", "cwInteractionOf"]

btnStopClick = do 
  liftIO js_stop


runOrError :: Workspace -> IO ()
runOrError ws = do
        (code,errors) <- workspaceToCode ws
        case errors of
          ((Error msg block):es) -> do 
                                      putStrLn $ T.unpack msg
                                      setWarningText block msg
                                      addErrorSelect block
                                      js_removeErrorsDelay
                                      setErrorMessage (T.unpack msg)

          [] -> do
            liftIO $ js_updateEditor (pack code)
            liftIO $ js_cwcompile (pack code)


-- Update the hash on the workspace
-- Mainly so that broken programs can be shared
updateCode :: Workspace -> IO ()
updateCode ws = do
        (code,errors) <- workspaceToCode ws
        liftIO $ js_updateEditor (pack code)
        liftIO $ js_cwcompilesilent (pack code)


btnRunClick ws = do
  Just doc <- liftIO currentDocument
  liftIO $ updateCode ws
  blocks <- liftIO $ getTopBlocks ws
  (block, w) <- liftIO $ isWarning ws
  if T.length w > 0 then do
    setErrorMessage (T.unpack w)
    liftIO $ addErrorSelect block
    liftIO $ js_removeErrorsDelay
  else do
    if not $ containsProgramBlock blocks 
      then do 
        setErrorMessage "No Program block on the workspace"
      else do
        liftIO $ runOrError ws
    return ()
  where
    containsProgramBlock = any (\b -> getBlockType b `elem` programBlocks) 

hookEvent elementName evType func = do
  Just doc <- currentDocument
  Just btn <- getElementById doc elementName
  on (uncheckedCastTo HTMLButtonElement btn) evType func

help = do
      js_injectReadOnly (JStr.pack "blocklyDiv")
      liftIO setBlockTypes 

funblocks = do 
      Just doc <- currentDocument 
      Just body <- getBody doc
      workspace <- liftIO $ setWorkspace "blocklyDiv" "toolbox"
      liftIO $ disableOrphans workspace -- Disable disconnected non-top level blocks
      liftIO $ warnOnInputs workspace -- Display warning if inputs are disconnected
      hookEvent "btnRun" click (btnRunClick workspace)
      hookEvent "btnStop" click btnStopClick
      liftIO setBlockTypes -- assign layout and types of Blockly blocks
      liftIO $ addChangeListener workspace (onChange workspace)
      liftIO js_showEast
      liftIO js_openEast
      -- Auto start
      liftIO $ setRunFunc workspace
      -- when (T.length hash > 0) $ liftIO $ runOrError workspace
      return ()


main = do
  Just doc <- currentDocument
  mayTool <- getElementById doc "toolbox" 
  case mayTool of
    Just _ -> funblocks
    Nothing -> help

-- Update code in real time
onChange ws event = do 
                      (code, errs) <- workspaceToCode ws
                      js_updateEditor (pack code)


setRunFunc :: Workspace -> IO ()
setRunFunc ws = do
      cb <- syncCallback ContinueAsync (do 
                                        runOrError ws) 
      js_setRunFunc cb

-- FFI

#ifdef ghcjs_HOST_OS

-- call blockworld.js compile
foreign import javascript unsafe "compile($1)"
  js_cwcompile :: JStr.JSString -> IO ()

foreign import javascript unsafe "compile($1,true)"
  js_cwcompilesilent :: JStr.JSString -> IO ()

-- call blockworld.js run
-- run (xmlHash, codeHash, msg, error)
foreign import javascript unsafe "run()"
  js_cwrun :: JStr.JSString -> JStr.JSString -> JStr.JSString -> Bool -> IO ()

-- call blockworld.js updateUI
foreign import javascript unsafe "updateUI()"
  js_updateUI :: IO ()

-- funnily enough, If I'm calling run "" "" "" False I get errors
foreign import javascript unsafe "run('','','',false)"
  js_stop :: IO ()

foreign import javascript unsafe "run('','',$1,true)"
  js_stopErr :: JStr.JSString -> IO ()

foreign import javascript unsafe "updateEditor($1)"
  js_updateEditor :: JStr.JSString -> IO ()

foreign import javascript unsafe "setTimeout(removeErrors,5000)"
  js_removeErrorsDelay :: IO ()

foreign import javascript unsafe "window.mainLayout.show('east')"
  js_showEast :: IO ()

foreign import javascript unsafe "window.mainLayout.open('east')"
  js_openEast :: IO ()

foreign import javascript unsafe "Blockly.inject($1, {});"
  js_injectReadOnly :: JStr.JSString -> IO Workspace

foreign import javascript unsafe "runFunc = $1"
  js_setRunFunc :: Callback.Callback a -> IO ()

#else

js_cwcompile :: JStr.JSString -> Prelude.IO ()
js_cwcompile = Prelude.error "GHCJS required"

js_cwcompilesilent :: JStr.JSString -> Prelude.IO ()
js_cwcompilesilent = Prelude.error "GHCJS required"

js_cwrun :: JStr.JSString -> JStr.JSString -> JStr.JSString -> Bool -> Prelude.IO ()
js_cwrun = Prelude.error "GHCJS required"

js_updateUI :: Prelude.IO ()
js_updateUI = Prelude.error "GHCJS required"

js_stop :: Prelude.IO ()
js_stop = Prelude.error "GHCJS required"

js_stopErr :: JStr.JSString -> Prelude.IO ()
js_stopErr = Prelude.error "GHCJS required"

js_updateEditor :: JStr.JSString -> Prelude.IO ()
js_updateEditor = Prelude.error "GHCJS required"

js_removeErrorsDelay :: Prelude.IO ()
js_removeErrorsDelay = Prelude.error "GHCJS required"

js_showEast :: Prelude.IO ()
js_showEast = Prelude.error "GHCJS required"

js_openEast :: Prelude.IO ()
js_openEast = Prelude.error "GHCJS required"

js_injectReadOnly :: JStr.JSString -> Prelude.IO Workspace
js_injectReadOnly = Prelude.error "GHCJS required"

js_setRunFunc :: Callback.Callback a -> Prelude.IO ()
js_setRunFunc = Prelude.error "GHCJS required"

#endif
