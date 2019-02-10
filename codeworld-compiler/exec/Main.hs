{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

import CodeWorld.Compile
import CodeWorld.Compile.Base
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)

data Options = Options
    { source :: FilePath
    , output :: Maybe FilePath
    , errfile :: Maybe FilePath
    , mode :: String
    , baseSymbols :: Maybe String
    , baseURL :: Maybe String
    , genBase :: Bool
    , baseIgnore :: [String]
    , verbose :: Bool
    } deriving (Show)

main = execParser opts >>= \opts -> checkOptions opts >> runWithOptions opts
  where
    parser =
        Options <$>
        argument str (metavar "SourceFile" <> help "Location of input file") <*>
        optional
            (strOption
                (long "output" <> short 'o' <> metavar "OutputFile" <>
                 help "Location of output file")) <*>
        optional
            (strOption
                (long "error" <> short 'e' <> metavar "ErrorFile" <>
                 help "Location of error file")) <*>
        strOption
            (long "mode" <> short 'm' <> metavar "BuildMode" <>
             help "Enter the mode of compilation") <*>
        optional
            (strOption
                 (long "base-syms" <> short 's' <> metavar "BaseSyms" <>
                  help "Location of base symbol file")) <*>
        optional
            (strOption
                 (long "base-url" <> short 'u' <> metavar "BaseURL" <>
                  help "URL to dynamically load base JS bundle")) <*>
        switch (long "gen-base" <> short 'b' <> help "Generate a base bundle.") <*>
        many
            (strOption
                 (long "ignore-in-base" <> metavar "ModOrSymbol" <>
                  help "Ignore this module or symbol in base.")) <*>
        switch (long "verbose" <> short 'v' <> help "Enable verbose output..")
    opts = info parser mempty

checkOptions :: Options -> IO ()
checkOptions Options {..} = do
    when (output == Nothing && genBase) $ do
        hPutStrLn stderr ("Flag --gen-base requires output")
        exitFailure
    when ((baseSymbols == Nothing) /= (baseURL == Nothing)) $ do
        hPutStrLn stderr ("Flags --base-symbols and --base-url can't be used separately")
        exitFailure
    when (output == Nothing && baseSymbols /= Nothing) $ do
        hPutStrLn stderr ("Flag --base-symbols requires output")
        exitFailure
    when (genBase && baseSymbols == Nothing) $ do
        hPutStrLn stderr ("Flag --gen-base requires --base-symbols")
        exitFailure
    exists <- doesFileExist source
    when (not exists) $ do
        hPutStrLn stderr ("File not found: " ++ source)
        exitFailure

runWithOptions :: Options -> IO ()
runWithOptions opts@Options {..} = withSystemTempDirectory"cw_errout" $ \errdir -> do
    let err = case errfile of
            Nothing -> errdir </> "err.txt"
            Just err -> err
    result <-
        if genBase
            then compileBase opts err
            else compile opts err
    readFile err >>= hPutStrLn stderr
    case result of
        CompileSuccess -> exitSuccess
        _              -> exitFailure

compileBase :: Options -> FilePath -> IO CompileStatus
compileBase Options {..} err = do
    withSystemTempDirectory "genbase" $ \tmpdir -> do
        let linkMain = tmpdir </> "LinkMain.hs"
        let linkBase = tmpdir </> "LinkBase.hs"
        generateBaseBundle [source] (map T.pack baseIgnore) mode linkMain linkBase
        when verbose $ do
            hPutStrLn stderr "GENERATED LinkMain.hs as:"
            readFile linkMain >>= hPutStrLn stderr
            hPutStrLn stderr "========================="
            hPutStrLn stderr "GENERATED LinkBase.hs as:"
            readFile linkBase >>= hPutStrLn stderr
            hPutStrLn stderr "========================="
        let stage = GenBase "LinkBase" linkBase (fromJust output) (fromJust baseSymbols)
        compileSource stage linkMain err mode verbose

compile :: Options -> FilePath -> IO CompileStatus
compile opts@Options {..} err = do
    let stage =
            case (output, baseSymbols, baseURL) of
                (Nothing, _, _) -> ErrorCheck
                (Just out, Nothing, _) -> FullBuild out
                (Just out, Just syms, Just url) -> UseBase out syms url
    compileSource stage source err mode verbose
