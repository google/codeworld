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

module CodeWorld.Requirements.Framework where

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
import Language.Haskell.Exts
import System.Exit (ExitCode(..))
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process

import qualified Config as GHCParse
import qualified DynFlags as GHCParse
import qualified FastString as GHCParse
import qualified Fingerprint as GHCParse
import qualified GHC.LanguageExtensions.Type as GHCParse
import qualified HeaderInfo as GHCParse
import qualified HsExtension as GHCParse
import qualified HsSyn as GHCParse
import qualified HscTypes as GHCParse
import qualified Lexer as GHCParse
import qualified Panic as GHCParse
import qualified Parser as GHCParse
import qualified Platform as GHCParse
import qualified SrcLoc as GHCParse
import qualified StringBuffer as GHCParse

data GHCParsedCode = GHCParsed (GHCParse.HsModule GHCParse.GhcPs) | GHCNoParse

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

ghcParseCode :: GHCParse.DynFlags -> [String] -> Text -> GHCParsedCode
ghcParseCode flags exts src = do
    let buffer = GHCParse.stringToStringBuffer (T.unpack src)
        dflags = foldl' applyExtensionToFlags flags exts
        location = GHCParse.mkRealSrcLoc (GHCParse.mkFastString "program.hs") 1 1
        state    = GHCParse.mkPState dflags buffer location
    case GHCParse.unP GHCParse.parseModule state of
        GHCParse.POk _ (GHCParse.L _ mod) -> GHCParsed mod
        GHCParse.PFailed _ _ _            -> GHCNoParse

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
