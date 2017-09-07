{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Main where

import           Compile
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString as B
import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import           HIndent (reformat)
import           HIndent.Types (defaultConfig)
import qualified Network.SocketIO as SIO
import           Network.EngineIO.Snap (snapAPI)
import           Snap.Core
import qualified Snap.Http.Server as Snap
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath

import Collaboration
import Comment
import           DataUtil
import           Folder
import qualified Funblocks as FB
import           Model
import           SnapUtil

main :: IO ()
main = do
    hasClientId <- doesFileExist "web/clientId.txt"
    unless hasClientId $ do
        putStrLn "WARNING: Missing web/clientId.txt"
        putStrLn "User logins will not function properly!"

    clientId <- case hasClientId of
        True -> do
            txt <- T.readFile "web/clientId.txt"
            return (ClientId (Just (T.strip txt)))
        False -> return (ClientId Nothing)

    state <- initCollabServer
    socketIOHandler <- SIO.initialize snapAPI (collabServer state clientId)
    config <- Snap.commandLineConfig $
        Snap.setErrorLog  (Snap.ConfigFileLog "log/collab-error.log") $
        Snap.setAccessLog (Snap.ConfigFileLog "log/collab-access.log") $
        mempty
    Snap.httpServe config $ (processBody >> site socketIOHandler clientId) <|> site socketIOHandler clientId

site :: Snap () -> ClientId -> Snap ()
site socketIOHandler clientId =
    route ([
        ("compile",     compileHandler),
        ("loadSource",  loadSourceHandler),
        ("run",         runHandler),
        ("runJS",       runHandler),
        ("runMsg",      runMessageHandler),
        ("haskell",     serveFile "web/env.html"),
        ("indent",      indentHandler)
      ] ++
        (collabRoutes socketIOHandler clientId) ++
        (commentRoutes clientId) ++
        (folderRoutes clientId) ++
        (FB.funblockRoutes $ currToFB clientId)) <|>
        serveDirectory "web"
  where
    currToFB clientId = case clientId of
        ClientId a -> FB.ClientId a

compileHandler :: Snap ()
compileHandler = do
    mode <- getBuildMode
    Just source <- getParam "source"
    let programId = sourceToProgramId source
        deployId = sourceToDeployId source
    success <- liftIO $ do
        ensureProgramDir mode programId
        B.writeFile (buildRootDir mode </> sourceFile programId) source
        writeDeployLink mode deployId programId
        compileIfNeeded mode programId
    unless success $ modifyResponse $ setResponseCode 500
    modifyResponse $ setContentType "text/plain"
    let result = CompileResult (unProgramId programId) (unDeployId deployId)
    writeLBS (encode result)

loadSourceHandler :: Snap ()
loadSourceHandler = do
    mode <- getBuildMode
    programId <- getHashParam False mode
    modifyResponse $ setContentType "text/x-haskell"
    serveFile (buildRootDir mode </> sourceFile programId)

runHandler :: Snap ()
runHandler = do
    mode <- getBuildMode
    programId <- getHashParam True mode
    liftIO $ compileIfNeeded mode programId
    modifyResponse $ setContentType "text/javascript"
    serveFile (buildRootDir mode </> targetFile programId)

runMessageHandler :: Snap ()
runMessageHandler = do
    mode <- getBuildMode
    programId <- getHashParam False mode
    modifyResponse $ setContentType "text/plain"
    serveFile (buildRootDir mode </> resultFile programId)

indentHandler :: Snap ()
indentHandler = do
    Just source <- getParam "source"
    case reformat defaultConfig Nothing source of
      Left err -> do
        modifyResponse $ setResponseCode 500 . setContentType "text/plain"
        writeLBS $ LB.fromStrict $ BC.pack err
      Right res -> do
        modifyResponse $ setContentType "text/x-haskell"
        writeLBS $ toLazyByteString res

compileIfNeeded :: BuildMode -> ProgramId -> IO Bool
compileIfNeeded mode programId = do
    hasResult <- doesFileExist (buildRootDir mode </> resultFile programId)
    hasTarget <- doesFileExist (buildRootDir mode </> targetFile programId)
    if hasResult
        then return hasTarget
        else compileSource
                 FullBuild
                 (buildRootDir mode </> sourceFile programId)
                 (buildRootDir mode </> targetFile programId)
                 (buildRootDir mode </> resultFile programId)
                 (getMode mode)

getMode :: BuildMode -> String
getMode (BuildMode m) = m
