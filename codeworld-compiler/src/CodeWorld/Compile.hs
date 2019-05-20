{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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
import Data.List
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
import Text.Read (readMaybe)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

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
    checkDangerousSource
    ifSucceeding checkCodeConventions
    ifSucceeding compileCode
    ifSucceeding checkRequirements

    errPath <- gets compileOutputPath
    diags <- sort <$> gets compileErrors
    liftIO $ createDirectoryIfMissing True (takeDirectory errPath)
    liftIO $ B.writeFile errPath $ encodeUtf8 $ formatDiagnostics diags

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

    return $ ["program.hs"] ++ buildArgs mode ++ linkArgs

buildArgs :: SourceMode -> [String]
buildArgs "codeworld" =
    [ "-DGHCJS_BROWSER"
    , "-Wall"
    , "-Wdeferred-type-errors"
    , "-Wdeferred-out-of-scope-variables"
    , "-fno-warn-deprecated-flags"
    , "-fno-warn-amp"
    , "-fno-warn-missing-signatures"
    , "-fno-warn-incomplete-patterns"
    , "-fno-warn-unused-matches"
    , "-fdefer-type-errors"
    , "-fdefer-out-of-scope-variables"
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
    , "-XNoQuasiQuotes"
    , "-XNoTemplateHaskell"
    , "-XNoUndecidableInstances"
    , "-XOverloadedStrings"
    , "-XPackageImports"
    , "-XPatternGuards"
    , "-XParallelListComp"
    , "-XPartialTypeSignatures"
    , "-XRankNTypes"
    , "-XRebindableSyntax"
    , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    , "-XTypeOperators"
    , "-XViewPatterns"
    , "-XImplicitPrelude" -- MUST come after RebindableSyntax.
    , "-main-is"
    , "Main.program"
    ]
buildArgs "haskell" =
    [ "-DGHCJS_BROWSER"
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
  where messages = filter (/= "") $ T.splitOn "\n\n" (T.strip output)
        newDiags = [ parseDiagnostic msg | msg <- messages ]

parseDiagnostic :: Text -> Diagnostic
parseDiagnostic msg
  | ((_ : (readT -> Just ln)
        : (readT -> Just col)
        : body : _) : _) <-
        msg =~ ("^program.hs:([0-9]+):([0-9]+): ((.|\n)*)$" :: Text)
    = (srcSpanFrom ln ln col col, CompileSuccess, T.unpack body)
  | ((_ : (readT -> Just ln)
        : (readT -> Just col1)
        : (readT -> Just col2)
        : body : _) : _) <-
        msg =~ ("^program.hs:([0-9]+):([0-9]+)-([0-9]+): ((.|\n)*)$" :: Text)
    = (srcSpanFrom ln ln col1 col2, CompileSuccess, T.unpack body)
  | ((_ : (readT -> Just ln1)
        : (readT -> Just col1)
        : (readT -> Just ln2)
        : (readT -> Just col2)
        : body : _) : _) <-
        msg =~ ("^program.hs:[(]([0-9]+),([0-9]+)[)]" <>
                "-[(]([0-9]+),([0-9]+)[)]: ((.|\n)*)$" :: Text)
    = (srcSpanFrom ln1 ln2 col1 col2, CompileSuccess, T.unpack body)
  | otherwise = (noSrcSpan, CompileSuccess, T.unpack msg ++ " (no loc)")
  where readT = readMaybe . T.unpack

srcSpanFrom :: Int -> Int -> Int -> Int -> SrcSpanInfo
srcSpanFrom l1 l2 c1 c2 =
    SrcSpanInfo (SrcSpan "program.hs" l1 c1 l2 c2) []

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
    ErrorCheck -> return ()
