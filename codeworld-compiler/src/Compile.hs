{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2017 The CodeWorld Authors. All rights reserved.

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

module Compile ( compileSource ) where

import           Control.Concurrent
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Monoid
import           ErrorSanitizer
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process
import           Text.Regex.TDFA

compileSource :: FilePath -> FilePath -> FilePath -> String -> IO Bool
compileSource src out err mode = checkDangerousSource src >>= \case
    True -> do
        B.writeFile err
            "Sorry, but your program refers to forbidden language features."
        return False
    False -> withSystemTempDirectory "buildSource" $ \tmpdir -> do
        copyFile src (tmpdir </> "program.hs")
        baseArgs <- case mode of
                "haskell"   -> return haskellCompatibleBuildArgs
                "codeworld" -> standardBuildArgs <$> hasOldStyleMain src
        let ghcjsArgs = baseArgs ++ [ "program.hs" ]
        runCompiler tmpdir userCompileMicros ghcjsArgs >>= \case
            Nothing -> return False
            Just output -> do
                let filteredOutput = case mode of
                        "haskell"   -> output
                        "codeworld" -> filterOutput output
                        _           -> output
                B.writeFile err filteredOutput

                let target = tmpdir </> "program.jsexe"
                hasTarget <- doesFileExist (target </> "rts.js")
                when hasTarget $ do
                    rtsCode <- B.readFile $ target </> "rts.js"
                    libCode <- B.readFile $ target </> "lib.js"
                    outCode <- B.readFile $ target </> "out.js"
                    B.writeFile out (rtsCode <> libCode <> outCode)
                    return ()

                return hasTarget

userCompileMicros :: Int
userCompileMicros = 45 * 1000000

checkDangerousSource :: FilePath -> IO Bool
checkDangerousSource dir = do
    contents <- B.readFile dir
    return $ matches contents ".*TemplateHaskell.*" ||
             matches contents ".*QuasiQuotes.*" ||
             matches contents ".*glasgow-exts.*"

hasOldStyleMain :: FilePath -> IO Bool
hasOldStyleMain fname = do
    contents <- B.readFile fname
    return (matches contents "(^|\\n)main[ \\t]*=")

matches :: ByteString -> ByteString -> Bool
matches txt pat = txt =~ pat

runCompiler :: FilePath -> Int -> [String] -> IO (Maybe ByteString)
runCompiler dir micros args = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc "ghcjs" args) {
            cwd       = Just dir,
            std_in    = CreatePipe,
            std_out   = CreatePipe,
            std_err   = CreatePipe,
            close_fds = True }

    hClose inh
    result <- withTimeout micros $ B.hGetContents errh
    hClose outh

    terminateProcess pid
    _ <- waitForProcess pid

    return result

standardBuildArgs :: Bool -> [String]
standardBuildArgs True = [
    "-dedupe",
    "-Wall",
    "-O2",
    "-fno-warn-deprecated-flags",
    "-fno-warn-amp",
    "-fno-warn-missing-signatures",
    "-fno-warn-incomplete-patterns",
    "-fno-warn-unused-matches",
    "-hide-package", "base",
    "-package", "codeworld-base",
    "-XBangPatterns",
    "-XDisambiguateRecordFields",
    "-XEmptyDataDecls",
    "-XExistentialQuantification",
    "-XForeignFunctionInterface",
    "-XGADTs",
    "-XJavaScriptFFI",
    "-XKindSignatures",
    "-XLiberalTypeSynonyms",
    "-XNamedFieldPuns",
    "-XNoMonomorphismRestriction",
    "-XNoQuasiQuotes",
    "-XNoTemplateHaskell",
    "-XNoUndecidableInstances",
    "-XOverloadedStrings",
    "-XPackageImports",
    "-XParallelListComp",
    "-XPatternGuards",
    "-XRankNTypes",
    "-XRebindableSyntax",
    "-XRecordWildCards",
    "-XScopedTypeVariables",
    "-XTypeOperators",
    "-XViewPatterns",
    "-XImplicitPrelude"  -- MUST come after RebindableSyntax.
    ]
standardBuildArgs False = standardBuildArgs True ++ [
    "-main-is", "Main.program"
    ]

haskellCompatibleBuildArgs :: [String]
haskellCompatibleBuildArgs = [
    "-dedupe",
    "-Wall",
    "-O2",
    "-package", "codeworld-api"
    ]

withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout micros action = do
    result <- newEmptyMVar
    killer <- forkIO $ threadDelay micros >> putMVar result Nothing
    runner <- forkIO $ action >>= putMVar result . Just
    r <- takeMVar result
    killThread killer
    killThread runner
    return r

