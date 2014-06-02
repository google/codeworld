{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    route [
      ("compile", compileHandler)
    ] <|>
    serveDirectory "web"

compileHandler :: Snap ()
compileHandler = do
    writeBS "Compiler not yet implemented."
