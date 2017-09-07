{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
  Copyright 2017 The CodeWorld Authors. All rights reserved.

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

import           Compile
import           GenBase

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Temp (withSystemTempDirectory)

data Options = Options { source      :: FilePath,
                         output      :: FilePath,
                         err         :: FilePath,
                         mode        :: String,
                         baseSymbols :: Maybe String,
                         genBase     :: Bool,
                         baseIgnore  :: [String]
                       } deriving (Show)

main = execParser opts >>= \opts -> checkOptions opts >> runWithOptions opts
    where
        parser = Options <$> argument str        (  metavar "SourceFile"
                                                 <> help "Location of input file")
                         <*> strOption           (  long "output"
                                                 <> short 'o'
                                                 <> metavar "OutputFile"
                                                 <> help "Location of output file")
                         <*> strOption           (  long "error"
                                                 <> short 'e'
                                                 <> metavar "ErrorFile"
                                                 <> help "Location of error file")
                         <*> strOption           (  long "mode"
                                                 <> short 'm'
                                                 <> metavar "BuildMode"
                                                 <> help "Enter the mode of compilation")
                         <*> optional (strOption (  long "base-syms"
                                                 <> short 's'
                                                 <> metavar "BaseSyms"
                                                 <> help "Location of base symbol file"))
                         <*> switch              (  long "gen-base"
                                                 <> short 'b'
                                                 <> help "Generate a base bundle.")
                         <*> many (strOption     (  long "ignore-in-base"
                                                 <> metavar "ModOrSymbol"
                                                 <> help "Ignore this module or symbol in base."))
        opts = info parser mempty

checkOptions :: Options -> IO ()
checkOptions Options{..} = do
    when (genBase && baseSymbols == Nothing) $ do
        hPutStrLn stderr ("Flag --gen-base requires --base-symbols")
        exitFailure
    exists <- doesFileExist source
    when (not exists) $ do
        hPutStrLn stderr ("File not found: " ++ source)
        exitFailure

runWithOptions :: Options -> IO ()
runWithOptions opts@Options{..} = do
    success <- if genBase then compileBase opts else compile opts
    readFile err >>= hPutStrLn stderr
    if success then exitSuccess else exitFailure

compileBase :: Options -> IO Bool
compileBase Options{..} = do
    withSystemTempDirectory "genbase" $ \tmpdir -> do
        let linkMain = tmpdir </> "LinkMain.hs"
        let linkBase = tmpdir </> "LinkBase.hs"
        generateBaseBundle source (map T.pack baseIgnore) mode linkMain linkBase
        let stage = GenBase "LinkBase" linkBase (fromJust baseSymbols)
        compileSource stage linkMain output err mode

compile :: Options -> IO Bool
compile opts@Options{..} = do
    let stage = case baseSymbols of
            Nothing   -> FullBuild
            Just syms -> UseBase syms
    compileSource stage source output err mode
