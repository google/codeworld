{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module Blockly.Generator ( Workspace )
  where

import GHCJS.Types

newtype Workspace = Workspace JSVal

