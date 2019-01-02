{-# LANGUAGE BangPatterns #-}
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

module CodeWorld.Compile.Framework where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
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
import Language.Haskell.Exts
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
    deriving (Eq, Show, Ord)

data CompileState = CompileState {
    compileMode         :: SourceMode,
    compileStage        :: Stage,
    compileSourcePath   :: FilePath,
    compileOutputPath   :: FilePath,
    compileVerbose      :: Bool,
    compileTimeout      :: Int,
    compileStatus       :: CompileStatus,
    compileErrors       :: [Diagnostic],
    compileReadSource   :: Maybe ByteString,
    compileParsedSource :: Maybe ParsedCode
    }
    deriving Show

type MonadCompile m = (MonadState CompileState m, MonadIO m, MonadMask m)

type SourceMode = String  -- typically "codeworld" or "haskell"

type Diagnostic = (SrcSpanInfo, CompileStatus, String)

data ParsedCode = Parsed (Module SrcSpanInfo) | NoParse deriving Show

getSourceCode :: MonadCompile m => m ByteString
getSourceCode = do
    cached <- gets compileReadSource
    case cached of
        Just source -> return source
        Nothing -> do
            src <- gets compileSourcePath
            source <- liftIO $ B.readFile src
            modify $ \state -> state { compileReadSource = Just source }
            return source

getParsedCode :: MonadCompile m => m ParsedCode
getParsedCode = do
    cached <- gets compileParsedSource
    case cached of
        Just parsed -> return parsed
        Nothing -> do
            source <- getSourceCode
            parsed <- parseCode [] (decodeUtf8 source)
            modify $ \state -> state { compileParsedSource = Just parsed }
            return parsed

codeworldModeExts :: [String]
codeworldModeExts =
    [ "BangPatterns"
    , "DisambiguateRecordFields"
    , "EmptyDataDecls"
    , "ExistentialQuantification"
    , "ForeignFunctionInterface"
    , "GADTs"
    , "JavaScriptFFI"
    , "KindSignatures"
    , "LiberalTypeSynonyms"
    , "NamedFieldPuns"
    , "NoMonomorphismRestriction"
    , "NoQuasiQuotes"
    , "NoTemplateHaskell"
    , "NoUndecidableInstances"
    , "OverloadedStrings"
    , "PackageImports"
    , "ParallelListComp"
    , "PartialTypeSignatures"
    , "PatternGuards"
    , "RankNTypes"
    , "RebindableSyntax"
    , "RecordWildCards"
    , "ScopedTypeVariables"
    , "TypeOperators"
    , "ViewPatterns"
    ]

parseCode :: MonadCompile m => [String] -> Text -> m ParsedCode
parseCode extraExts src = do
    sourceMode <- gets compileMode
    let result = parseFileContentsWithMode mode (T.unpack src)
        modeExts | sourceMode == "codeworld" = codeworldModeExts
             | otherwise = []
        extVals = [ parseExtension ext | ext <- modeExts ++ extraExts ]
        mode = defaultParseMode { parseFilename = "program.hs",
                                  extensions = extVals }

    return $ case result of
        ParseOk mod -> Parsed mod
        ParseFailed _ _ -> NoParse

addDiagnostics :: MonadCompile m => [Diagnostic] -> m ()
addDiagnostics diags = modify $ \state -> state {
    compileErrors = compileErrors state ++ diags,
    compileStatus = maximum
        (compileStatus state : map (\(_, s, _) -> s) diags)
    }

failCompile :: MonadCompile m => m ()
failCompile = do
    modify $ \state -> state {
        compileStatus = max CompileError (compileStatus state) }

ifSucceeding :: MonadCompile m => m () -> m ()
ifSucceeding m = do
    status <- gets compileStatus
    when (status == CompileSuccess) m

withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout micros action = do
    result <- newEmptyMVar
    killer <- forkIO $ threadDelay micros >> putMVar result Nothing
    runner <- forkIO $ action >>= putMVar result . Just
    r <- takeMVar result
    killThread killer
    killThread runner
    return r

-- Runs a command, where if the thread terminates, the subprocess is automatically
-- killed.
runSync :: FilePath -> String -> [String] -> IO (ExitCode, Text)
runSync dir cmd args = mask $ \restore -> do
    (Nothing, Just outh, Just errh, pid) <-
        createProcess
            (proc cmd args)
            { cwd = Just dir
            , std_in = NoStream
            , std_out = CreatePipe
            , std_err = CreatePipe
            , close_fds = True
            }
    let cleanup (e :: SomeException) = terminateProcess pid >> throwM e
    handle cleanup $ restore $ do
        result <- decodeUtf8 <$> B.hGetContents errh
        hClose outh
        exitCode <- waitForProcess pid
        return (exitCode, result)

formatLocation :: SrcSpanInfo -> String
formatLocation spn@(SrcSpanInfo s _)
  | spn == noSrcSpan = ""
  | otherwise        = "program.hs:" ++ show line ++ ":" ++ show col ++ ": "
  where
    fn = srcSpanFilename s
    line = srcSpanStartLine s
    col = srcSpanStartColumn s

srcSpanFor :: Text -> Int -> Int -> SrcSpanInfo
srcSpanFor src off len =
    SrcSpanInfo (SrcSpan "program.hs" ln1 col1 ln2 col2) []
  where (_, ln1, col1) = T.foldl' next (off, 1, 1) pre
        (_, ln2, col2) = T.foldl' next (len, ln1, col1) mid

        (pre, post) = T.splitAt off src
        mid = T.take len post

        next (!n, !ln, !col) '\r' = (n - 1, ln, col)
        next (!n, !ln, !col) '\n' = (n - 1, ln + 1, 1)
        next (!n, !ln, !col) '\t' = (n - 1, ln, col + 8 - (col - 1) `mod` 8)
        next (!n, !ln, !col) _    = (n - 1, ln, col + 1)
