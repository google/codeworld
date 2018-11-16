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

module CodeWorld.Compile
    ( compileSource
    , Stage(..)
    , CompileStatus(..)
    ) where

import CodeWorld.Compile.ParseCode
import Control.Applicative
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Data.List (intercalate)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import ErrorSanitizer
import System.Exit (ExitCode(..))
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process

data Stage
    = ErrorCheck
    | FullBuild FilePath  -- ^ Output file location
    | GenBase   String    -- ^ Base module name
                FilePath  -- ^ Base module file location
                FilePath  -- ^ Output file location
                FilePath  -- ^ Symbol file location
    | UseBase   FilePath  -- ^ Output file location
                FilePath  -- ^ Symbol file location
                String    -- ^ URL of the base JavaScript bundle
    deriving (Eq, Show)

data CompileStatus
    = CompileSuccess
    | CompileError
    | CompileAborted
    deriving (Eq, Show)

formatDiagnostics :: Text -> [String] -> Text
formatDiagnostics compilerOutput extraWarnings =
    T.intercalate "\n\n" (compilerOutput : map T.pack extraWarnings)

readUtf8 :: FilePath -> IO Text
readUtf8 f = decodeUtf8 <$> B.readFile f

writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 f = B.writeFile f . encodeUtf8

compileSource :: Stage -> FilePath -> FilePath -> String -> Bool -> IO CompileStatus
compileSource stage src err mode verbose = do
    contents <- readUtf8 src
    case runCustomDiagnostics mode contents of
        (True, messages) -> do
            createDirectoryIfMissing True (takeDirectory err)
            B.writeFile err $ encodeUtf8 (formatDiagnostics "" messages)
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
                        UseBase _ syms _ -> do
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
                                    "codeworld" -> rewriteErrors output
                                    _ -> output
                        B.writeFile err $ encodeUtf8 $
                            formatDiagnostics filteredOutput extraMessages
                        let target = tmpdir </> "program.jsexe"
                        when success $ case stage of
                            GenBase _ _ out syms -> do
                                rtsCode <- readUtf8 (target </> "rts.js")
                                libCode <- readUtf8 (target </> "lib.base.js")
                                outCode <- readUtf8 (target </> "out.base.js")
                                createDirectoryIfMissing True (takeDirectory out)
                                writeUtf8 out (rtsCode <> libCode <> outCode)
                                copyFile (target </> "out.base.symbs") syms
                            UseBase out _ baseURL -> do
                                let prefix = T.pack $
                                        "var el = document.createElement('script');" ++
                                        "el.type = 'text/javascript';" ++
                                        "el.src = '" ++ baseURL ++ "';" ++
                                        "el.async = false;" ++
                                        "el.onload = function() {"
                                let suffix = T.pack $
                                          "window.h$mainZCZCMainzimain = h$mainZCZCMainzimain;" ++
                                          "start();" ++
                                          "start = function() {};" ++
                                        "};" ++
                                        "document.body.appendChild(el);"
                                libCode <- readUtf8 (target </> "lib.js")
                                outCode <- readUtf8 (target </> "out.js")
                                createDirectoryIfMissing True (takeDirectory out)
                                writeUtf8 out (prefix <> libCode <> outCode <> suffix)
                            FullBuild out -> do
                                rtsCode <- readUtf8 (target </> "rts.js")
                                libCode <- readUtf8 (target </> "lib.js")
                                outCode <- readUtf8 (target </> "out.js")
                                createDirectoryIfMissing True (takeDirectory out)
                                writeUtf8 out (rtsCode <> libCode <> outCode)
                        return $ if success then CompileSuccess else CompileError

userCompileMicros :: Int
userCompileMicros = 20 * 1000000

runCompiler :: FilePath -> Int -> [String] -> Bool -> IO (Maybe (ExitCode, Text))
runCompiler dir micros args verbose = tryCompile ("-O" : args) <|> tryCompile args
  where
    tryCompile :: [String] -> IO (Maybe (ExitCode, Text))
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
        result <- withTimeout micros $ decodeUtf8 <$> B.hGetContents errh
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
