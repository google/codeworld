{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2015 Google Inc. All rights reserved.

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
import qualified Data.ByteString.Char8 as BC
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Regex.TDFA

import Util

generateBaseBundle :: IO ()
generateBaseBundle = do
    lns <- T.lines <$> T.readFile autocompletePath
    let exprs = catMaybes (map expression lns)
    let defs = [ "d" <> T.pack (show i) <> " = " <> e
                 | (i,e) <- zip [0 :: Int ..] exprs ]
    let src = "module LinkBase where\n" <> T.intercalate "\n" defs
    T.writeFile (buildRootDir </> "LinkBase.hs") src
    T.writeFile (buildRootDir </> "LinkMain.hs") "main = pictureOf(blank)"
    compileBase
  where expression t | T.null t           = Nothing
                     | isUpper (T.head t) = Nothing
                     | isLower (T.head t) = Just t
                     | otherwise          = Just ("(" <> t <> ")")

compileBase :: IO ()
compileBase = do
    let ghcjsArgs = commonGHCJSArgs ++ [
            "-generate-base", "LinkBase",
            "-o", "base",
            "LinkMain.hs"
          ]
    BC.putStrLn . fromJust =<< runCompiler (maxBound :: Int) ghcjsArgs
    return ()

compileIfNeeded :: Text -> IO Bool
compileIfNeeded programId = do
    hasResult <- doesFileExist (buildRootDir </> resultFile programId)
    if hasResult then return True else compileExistingSource programId

compileExistingSource :: Text -> IO Bool
compileExistingSource programId = checkDangerousSource programId >>= \case
    True -> do
        B.writeFile (buildRootDir </> resultFile programId) $
            "Sorry, but your program refers to forbidden language features."
        return False
    False -> do
        let ghcjsArgs = commonGHCJSArgs ++ [
                "-no-rts",
                "-no-stats",
                "-use-base", "base.jsexe/out.base.symbs",
                sourceFile programId
              ]
        runCompiler userCompileMicros ghcjsArgs >>= \case
            Nothing -> do
                removeFileIfExists (buildRootDir </> resultFile programId)
                removeFileIfExists (buildRootDir </> targetFile programId)
                return False
            Just output -> do
                B.writeFile (buildRootDir </> resultFile programId) output
                doesFileExist (buildRootDir </> targetFile programId)

userCompileMicros :: Int
userCompileMicros = 10 * 1000000

checkDangerousSource :: Text -> IO Bool
checkDangerousSource programId = do
    contents <- B.readFile (buildRootDir </> sourceFile programId)
    return $ matches contents ".*TemplateHaskell.*" ||
             matches contents ".*glasgow-exts.*"
  where
    matches :: ByteString -> ByteString -> Bool
    matches txt pat = txt =~ pat

runCompiler :: Int -> [String] -> IO (Maybe ByteString)
runCompiler micros args = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc "ghcjs" args) {
            cwd       = Just buildRootDir,
            std_in    = CreatePipe,
            std_out   = CreatePipe,
            std_err   = CreatePipe,
            close_fds = True }
    hClose inh

    result <- withTimeout micros $ do
        err <- B.hGetContents errh
        hClose outh
        return err

    terminateProcess pid
    _ <- waitForProcess pid

    return result

commonGHCJSArgs :: [String]
commonGHCJSArgs = [
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
