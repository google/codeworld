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
import CodeWorld.Compile.Stages
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
import Language.Haskell.Exts.SrcLoc
import System.Exit (ExitCode(..))
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process

formatDiagnostics :: [Diagnostic] -> Text
formatDiagnostics diags =
    T.intercalate "\n\n" (map formatDiagnostic diags)

formatDiagnostic :: Diagnostic -> Text
formatDiagnostic (loc, _, msg) =
    T.pack (formatLocation loc ++ msg)

readUtf8 :: FilePath -> IO Text
readUtf8 f = decodeUtf8 <$> B.readFile f

writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 f = B.writeFile f . encodeUtf8

compileSource
    :: Stage -> FilePath -> FilePath -> String -> Bool -> IO CompileStatus
compileSource stage src err mode verbose = fromMaybe CompileAborted <$>
    withTimeout timeout (compileStatus <$> execStateT build initialState)
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

userCompileMicros :: Int
userCompileMicros = 30 * 1000000

build :: MonadCompile m => m ()
build = do
    runCustomDiagnostics
    preCompileStatus <- gets compileStatus

    ifSucceeding compileCode

    err <- gets compileOutputPath
    liftIO $ createDirectoryIfMissing True (takeDirectory err)

    diags <- gets compileErrors
    liftIO $ B.writeFile err $ encodeUtf8 $ formatDiagnostics diags

compileCode :: MonadCompile m => m ()
compileCode = ifSucceeding $ withSystemTempDirectory "build" $ \tmpdir -> do
    ghcjsArgs <- prepareCompile tmpdir

    timeout <- gets compileTimeout
    verbose <- gets compileVerbose
    (exitCode, output) <- liftIO $ runCompiler tmpdir timeout ghcjsArgs verbose
    when (exitCode /= ExitSuccess) failCompile

    mode <- gets compileMode
    if mode == "codeworld"
        then addParsedDiagnostics (rewriteErrors output)
        else addParsedDiagnostics output

    ifSucceeding $ copyOutputFrom (tmpdir </> "program.jsexe")

prepareCompile :: MonadCompile m => FilePath -> m [String]
prepareCompile dir = do
    src <- gets compileSourcePath
    liftIO $ copyFile src (dir </> "program.hs")

    mode <- gets compileMode
    baseArgs <- case mode of
        "haskell" -> return haskellCompatibleBuildArgs
        "codeworld" ->
            standardBuildArgs <$> hasOldStyleMain <$> decodeUtf8 <$> getSourceCode

    stage <- gets compileStage
    linkArgs <- case stage of
        ErrorCheck -> return ["-fno-code"]
        FullBuild _ -> return ["-dedupe"]
        GenBase mod base _ _ -> do
            liftIO $ copyFile base (dir </> mod <.> "hs")
            return [mod <.> "hs", "-generate-base", mod]
        UseBase _ syms _ -> do
            liftIO $ copyFile syms (dir </> "out.base.symbs")
            return ["-dedupe", "-use-base", "out.base.symbs"]

    return $ ["program.hs"] ++ baseArgs ++ linkArgs

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

runCompiler :: FilePath -> Int -> [String] -> Bool -> IO (ExitCode, Text)
runCompiler dir timeout args verbose =
    withTimeout firstAttemptTimeout (attempt ("-O" : args))
        >>= maybe (attempt args) return
  where
    firstAttemptTimeout = timeout `div` 2

    attempt :: [String] -> IO (ExitCode, Text)
    attempt currentArgs = do
        when verbose $ hPutStrLn stderr $
            "COMMAND: ghcjs " ++ intercalate " " args
        (exitCode, result) <- runSync dir "ghcjs" args
        when verbose $ hPutStrLn stderr $ "RESULT: " ++ show exitCode
        return (exitCode, result)

addParsedDiagnostics :: MonadCompile m => Text -> m ()
addParsedDiagnostics output = addDiagnostics newDiags
  where messages = T.splitOn "\n\n" (T.strip output)
        newDiags = [ (noSrcSpan, CompileSuccess, T.unpack msg) | msg <- messages ]

copyOutputFrom :: MonadCompile m => FilePath -> m ()
copyOutputFrom target = gets compileStage >>= \stage -> case stage of
    GenBase _ _ out syms -> liftIO $ do
        rtsCode <- readUtf8 (target </> "rts.js")
        libCode <- readUtf8 (target </> "lib.base.js")
        outCode <- readUtf8 (target </> "out.base.js")
        createDirectoryIfMissing True (takeDirectory out)
        writeUtf8 out (rtsCode <> libCode <> outCode)
        createDirectoryIfMissing True (takeDirectory syms)
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
