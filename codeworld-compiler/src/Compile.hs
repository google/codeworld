{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-
  Copyright 2018 The CodeWorld Authors. All rights reserved.

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
module Compile
    ( compileSource
    , Stage(..)
    , CompileStatus(..)
    ) where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)
import Data.Monoid
import ErrorSanitizer
import System.Exit (ExitCode(..))
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process

import ParseCode

data Stage
    = ErrorCheck
    | FullBuild FilePath  -- ^ Output file location
    | GenBase   String    -- ^ Base module name
                FilePath  -- ^ Base module file location
                FilePath  -- ^ Output file location
                FilePath  -- ^ Symbol file location
    | UseBase   FilePath  -- ^ Output file location
                FilePath  -- ^ Symbol file location
    deriving (Eq, Show)

data CompileStatus
    = CompileSuccess
    | CompileError
    | CompileAborted
    deriving (Eq, Show)

formatDiagnostics ::  ByteString -> [String] -> ByteString
formatDiagnostics compilerOutput extraWarnings =
    B.intercalate "\n\n" (compilerOutput : map pack extraWarnings)

compileSource :: Stage -> FilePath -> FilePath -> String -> Bool -> IO CompileStatus
compileSource stage src err mode verbose = do
    contents <- B.readFile src
    case runCustomDiagnostics mode contents of
        (True, messages) -> do
            createDirectoryIfMissing True (takeDirectory err)
            B.writeFile err (formatDiagnostics "" messages)
            return CompileError
        (False, extraMessages) ->
            withSystemTempDirectory "build" $ \tmpdir -> do
                copyFile src (tmpdir </> "program.hs")
                baseArgs <-
                    case mode of
                        "haskell" -> return haskellCompatibleBuildArgs
                        "codeworld" -> return (standardBuildArgs (hasOldStyleMain contents))
                let timeout =
                      case stage of
                          GenBase _ _ _ _ -> maxBound :: Int
                          _               -> userCompileMicros
                linkArgs <-
                    case stage of
                        ErrorCheck -> return ["-fno-code"]
                        FullBuild _ -> return ["-dedupe"]
                        GenBase mod base _ _ -> do
                            copyFile base (tmpdir </> mod <.> "hs")
                            return [mod <.> "hs", "-generate-base", mod]
                        UseBase _ syms -> do
                            copyFile syms (tmpdir </> "out.base.symbs")
                            return ["-dedupe", "-use-base", "out.base.symbs"]
                let ghcjsArgs = ["program.hs"] ++ baseArgs ++ linkArgs
                runCompiler tmpdir timeout ghcjsArgs verbose >>= \case
                    Nothing -> return CompileAborted
                    Just (exitCode, output) -> do
                        let success = exitCode == ExitSuccess
                        createDirectoryIfMissing True (takeDirectory err)
                        let filteredOutput =
                                case mode of
                                    "codeworld" -> filterOutput output
                                    _ -> output
                        B.writeFile err (formatDiagnostics filteredOutput extraMessages)
                        let target = tmpdir </> "program.jsexe"
                        when success $ case stage of
                            GenBase _ _ out syms -> do
                                rtsCode <- B.readFile $ target </> "rts.js"
                                libCode <-
                                    B.readFile $ target </> "lib.base.js"
                                outCode <-
                                    B.readFile $ target </> "out.base.js"
                                createDirectoryIfMissing True (takeDirectory out)
                                B.writeFile
                                    out
                                    (rtsCode <> libCode <> outCode)
                                copyFile (target </> "out.base.symbs") syms
                            UseBase out _ -> do
                                libCode <- B.readFile $ target </> "lib.js"
                                outCode <- B.readFile $ target </> "out.js"
                                createDirectoryIfMissing True (takeDirectory out)
                                B.writeFile out (libCode <> outCode)
                            FullBuild out -> do
                                rtsCode <- B.readFile $ target </> "rts.js"
                                libCode <- B.readFile $ target </> "lib.js"
                                outCode <- B.readFile $ target </> "out.js"
                                createDirectoryIfMissing True (takeDirectory out)
                                B.writeFile
                                    out
                                    (rtsCode <> libCode <> outCode)
                        return $ if success then CompileSuccess else CompileError

userCompileMicros :: Int
userCompileMicros = 20 * 1000000

runCompiler :: FilePath -> Int -> [String] -> Bool -> IO (Maybe (ExitCode, ByteString))
runCompiler dir micros args verbose = do
    result <- tryCompile ("-O" : args)
    case result of
        Just _  -> return result
        Nothing -> tryCompile args
  where
    tryCompile :: [String] -> IO (Maybe (ExitCode, ByteString))
    tryCompile currentArgs = do
        when verbose $ hPutStrLn stderr $ "COMMAND: ghcjs " ++ intercalate " " args
        (Just inh, Just outh, Just errh, pid) <-
            createProcess
                (proc "ghcjs" args)
                { cwd = Just dir
                , std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , close_fds = True
                }
        hClose inh
        result <- withTimeout micros $ B.hGetContents errh
        hClose outh
        terminateProcess pid
        exitCode <- waitForProcess pid
        when verbose $ hPutStrLn stderr $ "RESULT: " ++ show exitCode
        return $ (exitCode,) <$> result

standardBuildArgs :: Bool -> [String]
standardBuildArgs True =
    [ "-DGHCJS_BROWSER"
    , "-Wall"
    , "-fno-warn-deprecated-flags"
    , "-fno-warn-amp"
    , "-fno-warn-missing-signatures"
    , "-fno-warn-incomplete-patterns"
    , "-fno-warn-unused-matches"
    , "-hide-package"
    , "base"
    , "-package"
    , "codeworld-base"
    , "-Wno-partial-type-signatures"
    , "-XBangPatterns"
    , "-XDisambiguateRecordFields"
    , "-XEmptyDataDecls"
    , "-XExistentialQuantification"
    , "-XForeignFunctionInterface"
    , "-XGADTs"
    , "-XJavaScriptFFI"
    , "-XKindSignatures"
    , "-XLiberalTypeSynonyms"
    , "-XNamedFieldPuns"
    , "-XNoMonomorphismRestriction"
    , "-XNoPatternGuards"
    , "-XNoQuasiQuotes"
    , "-XNoTemplateHaskell"
    , "-XNoUndecidableInstances"
    , "-XOverloadedStrings"
    , "-XPackageImports"
    , "-XParallelListComp"
    , "-XPartialTypeSignatures"
    , "-XRankNTypes"
    , "-XRebindableSyntax"
    , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    , "-XTypeOperators"
    , "-XViewPatterns"
    , "-XImplicitPrelude" -- MUST come after RebindableSyntax.
    ]
standardBuildArgs False = standardBuildArgs True ++ ["-main-is", "Main.program"]

haskellCompatibleBuildArgs :: [String]
haskellCompatibleBuildArgs =
    [ "-DGHCJS_BROWSER"
    , "-Wall"
    , "-package"
    , "codeworld-api"
    , "-package"
    , "QuickCheck"
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
