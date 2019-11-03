{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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

module CodeWorld.Compile.Framework where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (isPrefixOf, intercalate, foldl')
import Data.Map (Map)
import qualified Data.Map as M
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

import qualified "ghc-lib-parser" Config as GHCParse
import qualified "ghc-lib-parser" DynFlags as GHCParse
import qualified "ghc-lib-parser" FastString as GHCParse
import qualified "ghc-lib-parser" Fingerprint as GHCParse
import qualified "ghc-lib-parser" GHC.LanguageExtensions.Type as GHCParse
import qualified "ghc-lib-parser" HeaderInfo as GHCParse
import qualified "ghc-lib-parser" HsExtension as GHCParse
import qualified "ghc-lib-parser" HsSyn as GHCParse
import qualified "ghc-lib-parser" HscTypes as GHCParse
import qualified "ghc-lib-parser" Lexer as GHCParse
import qualified "ghc-lib-parser" Module as GHCParse
import qualified "ghc-lib-parser" Panic as GHCParse
import qualified "ghc-lib-parser" Parser as GHCParse
import qualified "ghc-lib-parser" GHC.Platform as GHCParse
import qualified "ghc-lib-parser" SrcLoc as GHCParse
import qualified "ghc-lib-parser" StringBuffer as GHCParse
import qualified "ghc-lib-parser" ToolSettings as GHCParse

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
    compileMode            :: SourceMode,
    compileStage           :: Stage,
    compileBuildDir        :: FilePath,
    compileSourcePaths     :: [FilePath],
    compileExtraSources    :: [FilePath],
    compileOutputPath      :: FilePath,
    compileVerbose         :: Bool,
    compileTimeout         :: Int,
    compileStatus          :: CompileStatus,
    compileErrors          :: [Diagnostic],
    compileMainSourcePath  :: Maybe FilePath,
    compileReadSource      :: Map FilePath ByteString,
    compileParsedSource    :: Map FilePath ParsedCode,
    compileGHCParsedSource :: Map FilePath GHCParsedCode
    }

type MonadCompile m = (MonadState CompileState m, MonadIO m, MonadMask m)

type SourceMode = String  -- typically "codeworld" or "haskell"

type Diagnostic = (SrcSpanInfo, CompileStatus, String)

data ParsedCode = Parsed (Module SrcSpanInfo) | NoParse deriving Show

data GHCParsedCode = GHCParsed (GHCParse.HsModule GHCParse.GhcPs) | GHCNoParse

getSourceCode :: MonadCompile m => FilePath -> m ByteString
getSourceCode src = do
    cached <- gets compileReadSource
    case M.lookup src cached of
        Just source -> return source
        Nothing -> do
            source <- liftIO $ B.readFile src
            modify $ \state -> state { compileReadSource = M.insert src source cached }
            return source

getParsedCode :: MonadCompile m => FilePath -> m ParsedCode
getParsedCode src = do
    cached <- gets compileParsedSource
    case M.lookup src cached of
        Just parsed -> return parsed
        Nothing -> do
            source <- getSourceCode src
            parsed <- parseCode ["TupleSections"] (decodeUtf8 source)
            modify $ \state -> state { compileParsedSource = M.insert src parsed cached }
            return parsed

getGHCParsedCode :: MonadCompile m => FilePath -> m GHCParsedCode
getGHCParsedCode src = do
    cached <- gets compileGHCParsedSource
    case M.lookup src cached of
        Just parsed -> return parsed
        Nothing -> do
            source <- getSourceCode src
            parsed <- ghcParseCode ["TupleSections"] (decodeUtf8 source)
            modify $ \state -> state { compileGHCParsedSource = M.insert src parsed cached }
            return parsed

getMainSourcePath :: MonadCompile m => m FilePath
getMainSourcePath = do
    mainPath <- gets compileMainSourcePath
    case mainPath of
        Just path -> return path
        Nothing -> do
            srcs <- gets compileSourcePaths
            parsed <- mapM getGHCParsedCode srcs
            let matched = [ src | (src, GHCParsed mod) <- zip srcs parsed
                                , isMainModName (GHCParse.hsmodName mod) ]
            let result = head (matched ++ srcs)
            modify $ \state -> state { compileMainSourcePath = Just result }
            return result
  where
    isMainModName Nothing = True
    isMainModName (Just (GHCParse.L _ modName))
        = GHCParse.moduleNameString modName == "Main"
    isMainModName _ = False

getMainSourceCode :: MonadCompile m => m ByteString
getMainSourceCode = getMainSourcePath >>= getSourceCode

getMainParsedCode :: MonadCompile m => m ParsedCode
getMainParsedCode = getMainSourcePath >>= getParsedCode

getMainGHCParsedCode :: MonadCompile m => m GHCParsedCode
getMainGHCParsedCode = getMainSourcePath >>= getGHCParsedCode

getDiagnostics :: MonadCompile m => m [Diagnostic]
getDiagnostics = do
    diags <- gets compileErrors
    return diags

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
    let exts | sourceMode == "codeworld" = codeworldModeExts ++ extraExts
             | otherwise                 = extraExts
        mode = defaultParseMode { parseFilename = "program.hs",
                                  extensions = map parseExtension exts }
    return $ case parseFileContentsWithMode mode (T.unpack src) of
        ParseOk mod -> Parsed mod
        ParseFailed _ _ -> NoParse

ghcExtensionsByName :: Map String GHCParse.Extension
ghcExtensionsByName = M.fromList [
    (GHCParse.flagSpecName spec, GHCParse.flagSpecFlag spec)
    | spec <- GHCParse.xFlags ]

applyExtensionToFlags :: GHCParse.DynFlags -> String -> GHCParse.DynFlags
applyExtensionToFlags dflags name
  | "No" `isPrefixOf` name =
        GHCParse.xopt_unset dflags $ fromJust $ M.lookup (drop 2 name) ghcExtensionsByName
  | otherwise =
        GHCParse.xopt_set dflags $ fromJust $ M.lookup name ghcExtensionsByName

ghcParseCode :: MonadCompile m => [String] -> Text -> m GHCParsedCode
ghcParseCode extraExts src = do
    sourceMode <- gets compileMode
    let buffer = GHCParse.stringToStringBuffer (T.unpack src)
        exts | sourceMode == "codeworld" = codeworldModeExts ++ extraExts
             | otherwise                 = extraExts
        defaultFlags = GHCParse.defaultDynFlags fakeSettings fakeLlvmConfig
        dflags = foldl' applyExtensionToFlags defaultFlags exts
    dflagsWithPragmas <- liftIO $
        fromMaybe dflags <$> parsePragmasIntoDynFlags dflags "program.hs" buffer
    let location = GHCParse.mkRealSrcLoc (GHCParse.mkFastString "program.hs") 1 1
        state    = GHCParse.mkPState dflagsWithPragmas buffer location
    return $ case GHCParse.unP GHCParse.parseModule state of
        GHCParse.POk _ (GHCParse.L _ mod) -> GHCParsed mod
        GHCParse.PFailed _                -> GHCNoParse

fakeSettings :: GHCParse.Settings
fakeSettings =
    GHCParse.Settings {
        GHCParse.sGhcNameVersion = GHCParse.GhcNameVersion {
            GHCParse.ghcNameVersion_programName = "ghcjs",
            GHCParse.ghcNameVersion_projectVersion = GHCParse.cProjectVersion
        },
        GHCParse.sFileSettings = GHCParse.FileSettings {},
        GHCParse.sTargetPlatform = GHCParse.Platform {
            GHCParse.platformWordSize = GHCParse.PW8,
            GHCParse.platformOS = GHCParse.OSUnknown,
            GHCParse.platformUnregisterised = True
        },
        GHCParse.sPlatformMisc = GHCParse.PlatformMisc {},
        GHCParse.sPlatformConstants = GHCParse.PlatformConstants {
            GHCParse.pc_DYNAMIC_BY_DEFAULT = False,
            GHCParse.pc_WORD_SIZE = 8
        },
        GHCParse.sToolSettings = GHCParse.ToolSettings {
            GHCParse.toolSettings_opt_P_fingerprint = GHCParse.fingerprint0
        }
    }

fakeLlvmConfig :: (GHCParse.LlvmTargets, GHCParse.LlvmPasses)
fakeLlvmConfig = ([], [])

parsePragmasIntoDynFlags :: GHCParse.DynFlags
                         -> FilePath
                         -> GHCParse.StringBuffer
                         -> IO (Maybe GHCParse.DynFlags)
parsePragmasIntoDynFlags dflags f src =
    GHCParse.handleGhcException (const $ return Nothing) $
        GHCParse.handleSourceError (const $ return Nothing) $ do
            let opts = GHCParse.getOptions dflags src f
            (dflagsWithPragmas, _, _) <- GHCParse.parseDynamicFilePragma dflags opts
            return $ Just dflagsWithPragmas

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

withTimeout :: forall a. Int -> IO a -> IO (Maybe a)
withTimeout micros action = do
    result :: MVar (Maybe (Either SomeException a)) <- newEmptyMVar
    killer <- forkIO $ threadDelay micros >> putMVar result Nothing
    runner <- forkIO $ do
        try action >>= putMVar result . Just
    r <- takeMVar result
    killThread killer
    killThread runner
    sequence $ either throwM return <$> r

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
formatLocation spn@(SrcSpanInfo (SrcSpan fn l1 c1 l2 c2) _)
  | spn == noSrcSpan = ""
  | l1 /= l2         = fn ++ ":(" ++ show l1 ++ "," ++ show c1 ++ ")-(" ++
                       show l2 ++ "," ++ show (max 1 (c2 - 1)) ++ ")"
  | c1 < c2 - 1      = fn ++ ":" ++ show l1 ++ ":" ++ show c1 ++ "-" ++
                       show (max 1 (c2 - 1))
  | otherwise        = fn ++ ":" ++ show l1 ++ ":" ++ show c1

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
