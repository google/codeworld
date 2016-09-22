{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2016 The CodeWorld Authors. All rights reserved.

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

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Regex.TDFA

import Util

compileIfNeeded :: BuildMode -> ProgramId -> IO Bool
compileIfNeeded mode programId = do
    hasResult <- doesFileExist (buildRootDir mode </> resultFile programId)
    hasTarget <- doesFileExist (buildRootDir mode </> targetFile programId)
    if hasResult then return hasTarget else compileExistingSource mode programId

compileExistingSource :: BuildMode -> ProgramId -> IO Bool
compileExistingSource mode programId = checkDangerousSource mode programId >>= \case
    True -> do
        B.writeFile (buildRootDir mode </> resultFile programId) $
            "Sorry, but your program refers to forbidden language features."
        return False
    False -> do
        let baseArgs = case mode of
                BuildMode "haskell"   -> haskellCompatibleBuildArgs
                _                     -> standardBuildArgs
            ghcjsArgs = baseArgs ++ [ sourceFile programId ]
        success <- runCompiler mode userCompileMicros ghcjsArgs >>= \case
            Nothing -> do
                removeFileIfExists (buildRootDir mode </> resultFile programId)
                removeFileIfExists (buildRootDir mode </> targetFile programId)
                return False
            Just output -> do
                B.writeFile (buildRootDir mode </> resultFile programId) output
                doesFileExist (buildRootDir mode </> targetFile programId)
        mapM_ (removeFileIfExists . (buildRootDir mode </>)) (auxiliaryFiles programId)
        return success

userCompileMicros :: Int
userCompileMicros = 10 * 1000000

checkDangerousSource :: BuildMode -> ProgramId -> IO Bool
checkDangerousSource mode programId = do
    contents <- B.readFile (buildRootDir mode </> sourceFile programId)
    return $ matches contents ".*TemplateHaskell.*" ||
             matches contents ".*QuasiQuotes.*" ||
             matches contents ".*glasgow-exts.*"
  where
    matches :: ByteString -> ByteString -> Bool
    matches txt pat = txt =~ pat

runCompiler :: BuildMode -> Int -> [String] -> IO (Maybe ByteString)
runCompiler mode micros args = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc "ghcjs" args) {
            cwd       = Just (buildRootDir mode),
            std_in    = CreatePipe,
            std_out   = CreatePipe,
            std_err   = CreatePipe,
            close_fds = True }

    hClose inh
    result <- withTimeout micros $ do
        err <- B.hGetContents errh
        return err
    hClose outh

    terminateProcess pid
    _ <- waitForProcess pid

    return result

standardBuildArgs :: [String]
standardBuildArgs = [
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

haskellCompatibleBuildArgs :: [String]
haskellCompatibleBuildArgs = [
    "-Wall",
    "-O2",
    "-package", "codeworld-api"
    ]
