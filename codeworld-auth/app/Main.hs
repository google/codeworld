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

module Main (main) where

import           CodeWorld.Auth.Secret
import           Data.Monoid ((<>))
import           Options.Applicative
                    ( Parser
                    , argument
                    , command
                    , execParser
                    , fullDesc
                    , header
                    , helper
                    , info
                    , metavar
                    , progDesc
                    , str
                    , subparser
                    )

data Options = Options Command

data Command = GenerateSecret FilePath

pOptions :: Parser Options
pOptions = Options <$> pCommand

pCommand :: Parser Command
pCommand = subparser
    ( command "generate-secret" (info pGenerateSecret (progDesc "Generate secret key"))
    )

pGenerateSecret :: Parser Command
pGenerateSecret = GenerateSecret <$> argument str (metavar "PATH")

main :: IO ()
main = parse >>= go
    where
        parse = execParser $ info (helper <*> pOptions) $
            fullDesc
            <> header "codeworld-auth"
            <> progDesc "CodeWorld auth tool"
        go (Options (GenerateSecret path)) = doGenerateSecret path

doGenerateSecret :: FilePath -> IO ()
doGenerateSecret path = do
    secret_ <- generateSecret
    writeSecret path secret_
