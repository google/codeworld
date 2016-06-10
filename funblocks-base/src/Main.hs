-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (
    main
) where

import Control.Applicative ((<$>))
import GHCJS.DOM
       (enableInspector, webViewGetDomDocument, runWebGUI, currentDocument, )
import GHCJS.DOM.Document (getBody, createElement,getElementById, createTextNode, Document(..))
-- import GHCJS.DOM.HTMLButtonElement (onClick)
import GHCJS.DOM.Element (setInnerHTML,setId, click, Element)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.EventM (on, mouseClientXY)
import GHCJS.DOM.Types (castToHTMLElement, castToHTMLButtonElement)
import qualified CodeWorld as CW
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import Data.JSString (unpack, pack)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans (liftIO)
import Blockly.Workspace
import Blocks.CodeGen
import Blocks.Types

data Type = TNumber | TString | TPicture | TNone
  deriving Show


foreign import javascript unsafe "compile($1)"
  js_cwcompile :: JSString -> IO ()

foreign import javascript unsafe "run()"
  js_cwrun :: IO ()

--btnBoomClick :: Document -> Element -> IO ()
btnBoomClick doc ws = do
        Just body <- getBody doc
        -- (x, y) <- mouseClientXY
        Just newParagraph <- createElement doc (Just "p")
        cwtext <- liftIO $ workspaceToCode ws
        text <- createTextNode doc cwtext
        appendChild newParagraph text
        appendChild body (Just newParagraph)
        return ()

btnRunClick ws = do
  liftIO $ print "btnRunClick"
  Just doc <- liftIO currentDocument
  code <- liftIO $ workspaceToCode ws
  liftIO $ js_cwcompile (pack code)
  liftIO $ js_cwrun
  Just genCode <- getElementById doc "genCode"
  setInnerHTML genCode (Just code)
  liftIO $ print "this is new"
  liftIO $ print code
  return ()



main = do 
          
      Just doc <- currentDocument 
      Just body <- getBody doc

      workspace <- liftIO $ setWorkspace "blocklyDiv" "toolbox"
      liftIO $ assignAll

      Just btnRun <- getElementById doc "btnRun" 
      on btnRun click (btnRunClick workspace)
      
      liftIO $ setBlockTypes -- assign layout and types of Blockly blocks

      return ()
    
-- main = runWebGUI $ \ webView -> do
--     enableInspector webView
--     Just doc <- webViewGetDomDocument webView
--     Just body <- getBody doc
--     setInnerHTML body (Just "<h1>Hello World</h1><div><button id='btnBoom'>test</button></div>")
--     Just btnBoom <- getElementById doc "btnBoom"  -- fmap castToHTMLButtonElement <$> getElementById doc "btnBoom"
--     on btnBoom click (btnBoomClick doc) 
--     return ()
