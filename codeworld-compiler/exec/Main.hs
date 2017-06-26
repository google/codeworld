{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

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

import Options.Applicative
import Control.Monad (join)
import Data.Monoid ((<>))
import System.Environment
import System.Directory
import System.IO

import "codeworld-compiler" Compile as C

data Options = Options { source :: String, 
                         err :: String, 
                         output :: String, 
                         mode :: String
                       } deriving (Show)

main = execParser opts >>= runWithOptions
    where
        parser = Options <$> argument str (  metavar "SourceFile" 
                                          <> help "Location of source file" )
                         <*> strOption    (  long "output" 
                                          <> short 'o' 
                                          <> metavar "OutputFile" 
                                          <> help "Location of output file" )
                         <*> strOption    (  long "error" 
                                          <> short 'e' 
                                          <> metavar "ErrorFile"  
                                          <> help "Location of error file" )
                         <*> strOption    (  long "mode" 
                                          <> short 'm' 
                                          <> metavar "BuildMode"  
                                          <> help "Enter the mode of compilation" )
        opts = info parser mempty

runWithOptions :: Options -> IO ()
runWithOptions opts = do
    fileExists <- doesFileExist (source opts)
    if fileExists then do
        compileOutput <- extractSource (source opts) (output opts) (err opts) (mode opts)
        return ()
        else putStrLn $ "File not found:" ++ (show (source opts))

extractSource :: String -> String -> String -> String -> IO Bool
extractSource  source out err mode = do 
    res <- C.compileSource source out err mode
    case res of 
        True -> return True
        False -> do
            errFile <- readFile err
            hPutStrLn stderr (show errFile)
            return False
