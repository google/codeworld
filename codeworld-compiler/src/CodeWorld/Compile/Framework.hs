{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

{-
  Copyright 2020 The CodeWorld Authors. All rights reserved.

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

import qualified "ghc" Config as GHC
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Functor.Identity
import Data.Generics
import Data.List (foldl', intercalate, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as T
import qualified "ghc" DynFlags as GHC
import ErrorSanitizer
import qualified "ghc" FastString as GHC
import qualified "ghc" Fingerprint as GHC
import qualified "ghc-boot-th" GHC.LanguageExtensions.Type as GHC
import qualified "ghc" HeaderInfo as GHC
import qualified "ghc" HsExtension as GHC
import qualified "ghc" HsSyn as GHC
import qualified "ghc" HscTypes as GHC
import Language.Haskell.Exts
import qualified "ghc" Lexer as GHC
import qualified "ghc" Module as GHC
import qualified "ghc" Outputable as GHC
import qualified "ghc" Panic as GHC
import qualified "ghc" Parser as GHC
import qualified "ghc" Platform as GHC
import qualified "ghc" SrcLoc as GHC
import qualified "ghc" StringBuffer as GHC
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

data Stage
  = ErrorCheck
  | -- | Output file location
    FullBuild FilePath
  | GenBase
      String
      -- ^ Base module name
      FilePath
      -- ^ Base module file location
      FilePath
      -- ^ Output file location
      FilePath
      -- ^ Symbol file location
  | UseBase
      FilePath
      -- ^ Output file location
      FilePath
      -- ^ Symbol file location
      String
      -- ^ URL of the base JavaScript bundle
  deriving (Eq, Show)

data CompileStatus
  = CompileSuccess
  | CompileError
  | CompileAborted
  deriving (Eq, Show, Ord)

data CompileState = CompileState
  { compileMode :: SourceMode,
    compileStage :: Stage,
    compileBuildDir :: FilePath,
    compileSourcePaths :: [FilePath],
    compileModuleFinder :: String -> IO (Maybe FilePath),
    compileOutputPath :: FilePath,
    compileVerbose :: Bool,
    compileTimeout :: Int,
    compileStatus :: CompileStatus,
    compileErrors :: [Diagnostic],
    compileMainSourcePath :: Maybe FilePath,
    compileReadSource :: Map FilePath ByteString,
    compileParsedSource :: Map FilePath ParsedCode,
    compileGHCParsedSource :: Map FilePath GHCParsedCode,
    compileImportLocations :: Map FilePath SrcSpanInfo
  }

type MonadCompile m = (MonadState CompileState m, MonadIO m, MonadMask m)

type SourceMode = String -- typically "codeworld" or "haskell"

type Diagnostic = (SrcSpanInfo, CompileStatus, String)

data ParsedCode = Parsed (Module SrcSpanInfo) | NoParse
  deriving (Typeable, Data, Show)

data GHCParsedCode = GHCParsed (GHC.HsModule GHC.GhcPs) | GHCNoParse
  deriving (Typeable, Data)

getSourceCode :: MonadCompile m => FilePath -> m ByteString
getSourceCode src = do
  cached <- gets compileReadSource
  case M.lookup src cached of
    Just source -> return source
    Nothing -> do
      source <- liftIO $ B.readFile src
      modify $ \state -> state {compileReadSource = M.insert src source cached}
      return source

gmapT' :: GenericT -> GenericT
gmapT' f x0 = runIdentity (gfoldl k Identity x0)
  where
    k :: Data d => Identity (d -> b) -> d -> Identity b
    k (Identity c) x = Identity $! c $! f x

strictify :: GenericT
strictify = gmapT' strictify

getParsedCode :: MonadCompile m => FilePath -> m ParsedCode
getParsedCode src = do
  cached <- gets compileParsedSource
  case M.lookup src cached of
    Just parsed -> return parsed
    Nothing -> do
      source <- getSourceCode src
      parsed <- parseCode ["TupleSections"] (decodeUtf8 source)

      parsed <-
        liftIO $
          catch
            (evaluate (strictify parsed))
            (\(e :: SomeException) -> (return NoParse))

      modify $ \state -> state {compileParsedSource = M.insert src parsed cached}
      return parsed

getGHCParsedCode :: MonadCompile m => FilePath -> m GHCParsedCode
getGHCParsedCode src = do
  cached <- gets compileGHCParsedSource
  case M.lookup src cached of
    Just parsed -> return parsed
    Nothing -> do
      source <- getSourceCode src
      parsed <- ghcParseCode ["TupleSections"] (decodeUtf8 source)

      parsed <-
        liftIO $
          catch
            (evaluate (strictify parsed))
            (\(e :: SomeException) -> (return GHCNoParse))

      modify $ \state -> state {compileGHCParsedSource = M.insert src parsed cached}
      return parsed

getMainSourcePath :: MonadCompile m => m FilePath
getMainSourcePath = do
  mainPath <- gets compileMainSourcePath
  case mainPath of
    Just path -> return path
    Nothing -> do
      srcs <- gets compileSourcePaths
      parsed <- mapM getGHCParsedCode srcs
      let matched =
            [ src | (src, GHCParsed mod) <- zip srcs parsed, isMainModName (GHC.hsmodName mod)
            ]
      let result = head (matched ++ srcs)
      modify $ \state -> state {compileMainSourcePath = Just result}
      return result
  where
    isMainModName Nothing = True
    isMainModName (Just (GHC.L _ modName)) =
      GHC.moduleNameString modName == "Main"

getMainSourceCode :: MonadCompile m => m ByteString
getMainSourceCode = getMainSourcePath >>= getSourceCode

getMainParsedCode :: MonadCompile m => m ParsedCode
getMainParsedCode = getMainSourcePath >>= getParsedCode

getMainGHCParsedCode :: MonadCompile m => m GHCParsedCode
getMainGHCParsedCode = getMainSourcePath >>= getGHCParsedCode

getMainModuleName :: MonadCompile m => m String
getMainModuleName = do
  result <- getMainGHCParsedCode
  return $ case result of
    GHCNoParse -> "Main"
    GHCParsed mod -> case GHC.hsmodName mod of
      Nothing -> "Main"
      Just (GHC.L _ name) -> GHC.moduleNameString name

getDiagnostics :: MonadCompile m => m [Diagnostic]
getDiagnostics = do
  diags <- gets compileErrors
  return diags

codeworldModeExts :: [String]
codeworldModeExts =
  [ "BangPatterns",
    "DisambiguateRecordFields",
    "EmptyDataDecls",
    "ExistentialQuantification",
    "ForeignFunctionInterface",
    "GADTs",
    "JavaScriptFFI",
    "KindSignatures",
    "LiberalTypeSynonyms",
    "NamedFieldPuns",
    "NoMonomorphismRestriction",
    "NoQuasiQuotes",
    "NoTemplateHaskell",
    "NoUndecidableInstances",
    "OverloadedStrings",
    "PackageImports",
    "ParallelListComp",
    "PartialTypeSignatures",
    "PatternGuards",
    "RankNTypes",
    "RebindableSyntax",
    "RecordWildCards",
    "ScopedTypeVariables",
    "TypeOperators",
    "ViewPatterns"
  ]

parseCode :: MonadCompile m => [String] -> Text -> m ParsedCode
parseCode extraExts src = do
  sourceMode <- gets compileMode
  let exts
        | sourceMode == "codeworld" = codeworldModeExts ++ extraExts
        | otherwise = extraExts
      mode =
        defaultParseMode
          { parseFilename = "program.hs",
            extensions = map parseExtension exts
          }
  return $ case parseFileContentsWithMode mode (T.unpack src) of
    ParseOk mod -> Parsed mod
    ParseFailed _ _ -> NoParse

ghcExtensionsByName :: Map String GHC.Extension
ghcExtensionsByName =
  M.fromList
    [ (GHC.flagSpecName spec, GHC.flagSpecFlag spec)
      | spec <- GHC.xFlags
    ]

applyExtensionToFlags :: GHC.DynFlags -> String -> GHC.DynFlags
applyExtensionToFlags dflags name
  | "No" `isPrefixOf` name =
    GHC.xopt_unset dflags $ fromJust $ M.lookup (drop 2 name) ghcExtensionsByName
  | otherwise =
    GHC.xopt_set dflags $ fromJust $ M.lookup name ghcExtensionsByName

ghcParseCode :: MonadCompile m => [String] -> Text -> m GHCParsedCode
ghcParseCode extraExts src = do
  sourceMode <- gets compileMode
  let buffer = GHC.stringToStringBuffer (T.unpack src)
      exts
        | sourceMode == "codeworld" = codeworldModeExts ++ extraExts
        | otherwise = extraExts
      dflags = foldl' applyExtensionToFlags fakeDynFlags exts
  dflagsWithPragmas <-
    liftIO $
      fromMaybe dflags <$> parsePragmasIntoDynFlags dflags "program.hs" buffer
  let location = GHC.mkRealSrcLoc (GHC.mkFastString "program.hs") 1 1
      state = GHC.mkPState dflagsWithPragmas buffer location
  return $ case GHC.unP GHC.parseModule state of
    GHC.POk _ (GHC.L _ mod) -> GHCParsed mod
    GHC.PFailed _ _ _ -> GHCNoParse

fakeDynFlags :: GHC.DynFlags
fakeDynFlags = GHC.defaultDynFlags fakeSettings fakeLlvmConfig

fakeSettings :: GHC.Settings
fakeSettings =
  GHC.Settings
    { GHC.sProgramName = "ghcjs",
      GHC.sProjectVersion = GHC.cProjectVersion,
      GHC.sTargetPlatform =
        GHC.Platform
          { GHC.platformWordSize = 8,
            GHC.platformOS = GHC.OSUnknown,
            GHC.platformUnregisterised = True
          },
      GHC.sPlatformConstants =
        GHC.PlatformConstants
          { GHC.pc_DYNAMIC_BY_DEFAULT = False,
            GHC.pc_WORD_SIZE = 8
          },
      GHC.sOpt_P_fingerprint = GHC.fingerprint0
    }

fakeLlvmConfig :: (GHC.LlvmTargets, GHC.LlvmPasses)
fakeLlvmConfig = ([], [])

parsePragmasIntoDynFlags ::
  GHC.DynFlags ->
  FilePath ->
  GHC.StringBuffer ->
  IO (Maybe GHC.DynFlags)
parsePragmasIntoDynFlags dflags f src =
  GHC.handleGhcException (const $ return Nothing) $
    GHC.handleSourceError (const $ return Nothing) $
      do
        let opts = GHC.getOptions dflags src f
        (dflagsWithPragmas, _, _) <- GHC.parseDynamicFilePragma dflags opts
        return $ Just dflagsWithPragmas

copyModuleWithTransform ::
  MonadCompile m =>
  FilePath ->
  (GHC.HsModule GHC.GhcPs -> GHC.HsModule GHC.GhcPs) ->
  m (Maybe FilePath)
copyModuleWithTransform f transform = do
  src <- liftIO $ decodeUtf8 <$> B.readFile f
  let commentsAndPragmas = filter (=~ ("\\s*{-.*-}\\s*" :: Text)) (T.lines src)
  parseResult <- ghcParseCode [] src
  case parseResult of
    GHCNoParse -> return Nothing
    GHCParsed mod -> do
      buildDir <- gets compileBuildDir
      liftIO $ do
        (out, h) <- openTempFile buildDir "imported.hs"
        T.hPutStrLn h (T.unlines commentsAndPragmas)
        GHC.printForUser fakeDynFlags h GHC.neverQualify $ GHC.ppr (transform mod)
        hClose h
        return (Just out)

addDiagnostics :: MonadCompile m => [Diagnostic] -> m ()
addDiagnostics diags = modify $ \state ->
  state
    { compileErrors = compileErrors state ++ diags,
      compileStatus =
        maximum
          (compileStatus state : map (\(_, s, _) -> s) diags)
    }

failCompile :: MonadCompile m => m ()
failCompile = do
  modify $ \state ->
    state
      { compileStatus = max CompileError (compileStatus state)
      }

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
        { cwd = Just dir,
          std_in = NoStream,
          std_out = CreatePipe,
          std_err = CreatePipe,
          close_fds = True
        }
  let cleanup (e :: SomeException) = terminateProcess pid >> throwM e
  handle cleanup $
    restore $ do
      (resultOut, resultErr) <-
        concurrently
          (decodeUtf8 <$> B.hGetContents outh)
          (decodeUtf8 <$> B.hGetContents errh)
      exitCode <- waitForProcess pid
      return (exitCode, resultOut <> "\n" <> resultErr)

formatLocation :: SrcSpanInfo -> String
formatLocation spn@(SrcSpanInfo (SrcSpan fn l1 c1 l2 c2) _)
  | spn == noSrcSpan = ""
  | l1 /= l2 =
    fn ++ ":(" ++ show l1 ++ "," ++ show c1 ++ ")-("
      ++ show l2
      ++ ","
      ++ show (max 1 (c2 - 1))
      ++ ")"
  | c1 < c2 - 1 =
    fn ++ ":" ++ show l1 ++ ":" ++ show c1 ++ "-"
      ++ show (max 1 (c2 - 1))
  | otherwise = fn ++ ":" ++ show l1 ++ ":" ++ show c1

srcSpanFor :: FilePath -> Text -> Int -> Int -> SrcSpanInfo
srcSpanFor f src off len =
  SrcSpanInfo (SrcSpan (takeFileName f) ln1 col1 ln2 col2) []
  where
    (_, ln1, col1) = T.foldl' next (off, 1, 1) pre
    (_, ln2, col2) = T.foldl' next (len, ln1, col1) mid
    (pre, post) = T.splitAt off src
    mid = T.take len post
    next (!n, !ln, !col) '\r' = (n - 1, ln, col)
    next (!n, !ln, !col) '\n' = (n - 1, ln + 1, 1)
    next (!n, !ln, !col) '\t' = (n - 1, ln, col + 8 - (col - 1) `mod` 8)
    next (!n, !ln, !col) _ = (n - 1, ln, col + 1)
