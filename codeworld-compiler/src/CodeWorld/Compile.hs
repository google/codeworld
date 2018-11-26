{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import CodeWorld.Compile.Framework
import CodeWorld.Compile.ParseCode
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (intercalate)
import Data.Maybe
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

formatDiagnostics :: Text -> [String] -> Text
formatDiagnostics compilerOutput extraWarnings =
    T.intercalate "\n\n" (compilerOutput : map T.pack extraWarnings)

readUtf8 :: FilePath -> IO Text
readUtf8 f = decodeUtf8 <$> B.readFile f

writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 f = B.writeFile f . encodeUtf8

compileSource
    :: Stage -> FilePath -> FilePath -> String -> Bool -> IO CompileStatus
compileSource stage src err mode verbose = fromMaybe CompileAborted <$>
    withTimeout timeout (compileStatus <$> execStateT compile initialState)
  where
    initialState = CompileState {
        compileMode = mode,
        compileStage = stage,
        compileSourcePath = src,
        compileOutputPath = err,
        compileVerbose = verbose,
        compileTimeout = timeout,
        compileStatus = CompileSuccess,
        compileErrors = [],
        compileReadSource = Nothing,
        compileParsedSource = Nothing
        }
    timeout = case stage of
        GenBase _ _ _ _ -> maxBound :: Int
        _               -> userCompileMicros

compile :: MonadCompile m => m ()
compile = do
    runCustomDiagnostics
    preCompileStatus <- gets compileStatus

    status <- case preCompileStatus of
        CompileSuccess -> do
            stage <- gets compileStage
            src <- gets compileSourcePath
            err <- gets compileOutputPath
            verbose <- gets compileVerbose
            mode <- gets compileMode
            timeout <- gets compileTimeout
            contents <- decodeUtf8 <$> getSourceCode

            extraMessages <- map (\(l, _, t) -> formatLocation l ++ t) <$> gets compileErrors
            withSystemTempDirectory "build" $ \tmpdir -> do
                liftIO $ copyFile src (tmpdir </> "program.hs")
                baseArgs <-
                    case mode of
                        "haskell" -> return haskellCompatibleBuildArgs
                        "codeworld" -> return (standardBuildArgs (hasOldStyleMain contents))
                linkArgs <-
                    case stage of
                        ErrorCheck -> return ["-fno-code"]
                        FullBuild _ -> return ["-dedupe"]
                        GenBase mod base _ _ -> do
                            liftIO $ copyFile base (tmpdir </> mod <.> "hs")
                            return [mod <.> "hs", "-generate-base", mod]
                        UseBase _ syms _ -> do
                            liftIO $ copyFile syms (tmpdir </> "out.base.symbs")
                            return ["-dedupe", "-use-base", "out.base.symbs"]
                let ghcjsArgs = ["program.hs"] ++ baseArgs ++ linkArgs
                liftIO (runCompiler tmpdir timeout ghcjsArgs verbose) >>= \case
                    Nothing -> return CompileAborted
                    Just (exitCode, output) -> do
                        let success = exitCode == ExitSuccess
                        liftIO $ createDirectoryIfMissing True (takeDirectory err)
                        let filteredOutput =
                                case mode of
                                    "codeworld" -> rewriteErrors output
                                    _ -> output
                        liftIO $ B.writeFile err $ encodeUtf8 $
                            formatDiagnostics filteredOutput extraMessages
                        let target = tmpdir </> "program.jsexe"
                        when success $ case stage of
                            GenBase _ _ out syms -> liftIO $ do
                                rtsCode <- readUtf8 (target </> "rts.js")
                                libCode <- readUtf8 (target </> "lib.base.js")
                                outCode <- readUtf8 (target </> "out.base.js")
                                createDirectoryIfMissing True (takeDirectory out)
                                writeUtf8 out (rtsCode <> libCode <> outCode)
                                copyFile (target </> "out.base.symbs") syms
                            UseBase out _ baseURL -> liftIO $ do
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
                            FullBuild out -> liftIO $ do
                                rtsCode <- readUtf8 (target </> "rts.js")
                                libCode <- readUtf8 (target </> "lib.js")
                                outCode <- readUtf8 (target </> "out.js")
                                createDirectoryIfMissing True (takeDirectory out)
                                writeUtf8 out (rtsCode <> libCode <> outCode)
                        return $ if success then CompileSuccess else CompileError
        _ -> do
            err <- gets compileOutputPath
            messages <- map (\(l, _, t) -> formatLocation l ++ t) <$> gets compileErrors
            liftIO $ createDirectoryIfMissing True (takeDirectory err)
            liftIO $ B.writeFile err $ encodeUtf8 (formatDiagnostics "" messages)
            return CompileError
    modify $ \state -> state { compileStatus = status }

userCompileMicros :: Int
userCompileMicros = 30 * 1000000

runCompiler :: FilePath -> Int -> [String] -> Bool -> IO (Maybe (ExitCode, Text))
runCompiler dir timeout args verbose =
    withTimeout firstAttemptTimeout (attempt ("-O" : args)) <|>
    Just <$> attempt args
  where
    firstAttemptTimeout = timeout `div` 2

    attempt :: [String] -> IO (ExitCode, Text)
    attempt currentArgs = do
        when verbose $ hPutStrLn stderr $
            "COMMAND: ghcjs " ++ intercalate " " args
        (exitCode, result) <- runSync dir "ghcjs" args
        when verbose $ hPutStrLn stderr $ "RESULT: " ++ show exitCode
        return (exitCode, result)

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
