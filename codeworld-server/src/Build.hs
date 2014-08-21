{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2014 Google Inc. All rights reserved.

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

module Build where

import           Control.Exception
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

import Util

localSourceFile :: ByteString -> FilePath
localSourceFile hashed = BC.unpack hashed ++ ".hs"

sourceFile :: ByteString -> FilePath
sourceFile hashed = "user" </> localSourceFile hashed

targetFile :: ByteString -> FilePath
targetFile hashed = "user" </> BC.unpack hashed ++ ".jsexe" </> "out.js"

resultFile :: ByteString -> FilePath
resultFile hashed = "user" </> BC.unpack hashed ++ ".err.txt"

compileIfNeeded :: ByteString -> IO Bool
compileIfNeeded hashed = do
    hasResult <- doesFileExist (resultFile hashed)
    hasTarget <- doesFileExist (targetFile hashed)
    if hasResult && hasTarget then return True else compileUserSource hashed

generateBaseBundle :: IO ()
generateBaseBundle = do
    let ghcjsArgs = commonGHCJSArgs ++ [
            "--generate-base=LinkBase",
            "-o", "base",
            "LinkMain.hs"
          ]
    BC.putStrLn . fromJust =<< runCompiler (maxBound :: Int) ghcjsArgs
    return ()

compileUserSource :: ByteString -> IO Bool
compileUserSource hashed = do
    let src = localSourceFile hashed
    let err = resultFile hashed
    let tgt = targetFile hashed

    let ghcjsArgs = commonGHCJSArgs ++ [
            "--no-rts",
            "--no-stats",
            "--use-base=base.jsexe/out.base.symbs",
            "./" ++ src
          ]
    result <- runCompiler userCompileMicros ghcjsArgs
    case result of
        Nothing     -> do
            -- A missing error file indicates that the compiler
            -- timed out, so delete if it exists.
            removeFileIfExists err
            return False
        Just output -> do
            B.writeFile err output
            doesFileExist tgt

userCompileMicros :: Int
userCompileMicros = 10 * 1000000

runCompiler :: Int -> [String] -> IO (Maybe ByteString)
runCompiler micros args = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc "ghcjs" args) {
            cwd       = Just "user",
            std_in    = CreatePipe,
            std_out   = CreatePipe,
            std_err   = CreatePipe,
            close_fds = True }
    hClose inh

    result <- withTimeout micros $ do
        err <- B.hGetContents errh
        evaluate (B.length err)
        hClose outh
        return err

    terminateProcess pid
    return result

commonGHCJSArgs :: [String]
commonGHCJSArgs = [
    "--no-native",
    "-Wall",
    "-O2",
    "-fno-warn-deprecated-flags",
    "-fno-warn-amp",
    "-fno-warn-missing-signatures",
    "-fno-warn-incomplete-patterns",
    "-fno-warn-unused-matches",
    "-hide-package", "base",
    "-package", "codeworld-base",
    "-XRebindableSyntax",
    "-XImplicitPrelude",
    "-XOverloadedStrings",
    "-XNoTemplateHaskell",
    "-XNoUndecidableInstances",
    "-XNoQuasiQuotes",
    "-XForeignFunctionInterface",
    "-XJavaScriptFFI",
    "-XParallelListComp",
    "-XDisambiguateRecordFields",
    "-XNoMonomorphismRestriction",
    "-XScopedTypeVariables",
    "-XBangPatterns",
    "-XPatternGuards",
    "-XViewPatterns",
    "-XRankNTypes",
    "-XExistentialQuantification",
    "-XKindSignatures",
    "-XEmptyDataDecls",
    "-XLiberalTypeSynonyms",
    "-XTypeOperators",
    "-XRecordWildCards",
    "-XNamedFieldPuns"
    ]
