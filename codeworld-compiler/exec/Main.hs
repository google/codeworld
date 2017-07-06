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
import           Control.Monad (join)
import           Data.Monoid ((<>))
import           Options.Applicative
import           System.Environment
import           System.Directory
import           System.IO

data Options = Options { source :: String, 
                         output :: String, 
                         err :: String, 
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
runWithOptions Options{..} = do
    fileExists <- doesFileExist source
    if fileExists 
      then do
        compileOutput <- extractSource source output err mode
        return ()
      else 
        putStrLn $ "File not found:" ++ (show source)

extractSource :: String -> String -> String -> String -> IO Bool
extractSource  source out err mode = do 
    res <- compileSource source out err mode
    case res of 
        True -> return True
        False -> do
            errMsg <- readFile err
            hPutStrLn stderr (show errMsg)
            return False
