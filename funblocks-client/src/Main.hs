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

import Control.Applicative ((<$>))
import GHCJS.DOM
       (currentDocument, )
import GHCJS.DOM.Document (getBody, getElementById, Document(..))
import GHCJS.DOM.Element (setInnerHTML, click, Element)
import GHCJS.DOM.EventM (on )
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import Data.JSString (pack)
import Control.Monad.Trans (liftIO)
import Blockly.Workspace
import Blocks.CodeGen
import Blocks.Types
import Blockly.Event
import Blockly.General
import Blockly.Block 

-- call blockworld.js compile
foreign import javascript unsafe "compile($1)"
  js_cwcompile :: JSString -> IO ()

-- call blockworld.js run
foreign import javascript unsafe "run()"
  js_cwrun :: IO ()

foreign import javascript unsafe "updateEditor($1)"
  js_updateEditor :: JSString -> IO ()


setErrorMessage msg = do
  Just doc <- liftIO currentDocument
  Just msgEl <- getElementById doc "message"
  setInnerHTML msgEl $ Just msg

btnRunClick ws = do
  Just doc <- liftIO currentDocument
  code <- liftIO $ workspaceToCode ws
  Just genCode <- getElementById doc "genCode"
  case code of
    "" -> setErrorMessage "Error: Disconnected Inputs"
    _ -> do
          liftIO $ js_cwcompile (pack code)
          liftIO js_cwrun
          liftIO $ js_updateEditor (pack code)
  return ()

-- test whether all blocks have codegen
allCodeGen = filter (\x -> not $ x `elem` getGenerationBlocks) getTypeBlocks 

main = do 
      Just doc <- currentDocument 
      Just body <- getBody doc
      liftIO $ putStrLn $ "Blocks that dont have codegen: " ++ (show allCodeGen)
      workspace <- liftIO $ setWorkspace "blocklyDiv" "toolbox"
      liftIO assignAll
      Just btnRun <- getElementById doc "btnRun" 
      on btnRun click (btnRunClick workspace)
      liftIO setBlockTypes -- assign layout and types of Blockly blocks
      -- liftIO $ addChangeListener workspace (onVarConnect workspace)
      liftIO $ addChangeListener workspace (onGeneral workspace)
      return ()

onGeneral workspace event = case getType event of
      MoveEvent e -> do
        let uuid = getBlockId e
        case getBlockById workspace uuid of
          Just block -> case (getOutputConnection block, isTopBlock workspace block) of
                          (Just _, True) -> setDisabled block True
                          _ -> setDisabled block False
          Nothing -> return ()
      _ -> return ()

-- change the color of a var block 
onVarConnect workspace event = do
  let eventtype = getType event
  case eventtype of
    -- ChangeEvent e -> putStrLn "testing change event" 
    -- CreateEvent e -> putStrLn "testing create event" 
    MoveEvent e -> do
        let uuid = getBlockId e
        let ablock = getBlockById workspace uuid 
        let bbs = do
                    block_ <- getBlockById workspace uuid
                    outBlock_ <- getOutputBlock block_
                    let tp = getBlockType outBlock_
                    return (block_,outBlock_,tp)
        case bbs of
          Just (block, outBlock,"letVar") -> do
                                      let col = getColour block
                                      setColour outBlock col
                                      --putStrLn tp
                                      return ()
          _ -> return ()
        return ()
    _ -> return ()
  return ()

