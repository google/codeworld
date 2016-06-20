{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2016 The CodeWorld Authors. All rights reserved.

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

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Conduit
import           Snap.Core
import           Snap.Http.Server (quickHttpServe)
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           System.Directory
import           System.FilePath

import Build
import Model
import Util

newtype ClientId = ClientId (Maybe T.Text) deriving (Eq)

main :: IO ()
main = do
    hasClientId <- doesFileExist "web/clientId.txt"
    when (not hasClientId) $ do
        putStrLn "WARNING: Missing web/clientId.txt"
        putStrLn "User logins will not function properly!"

    clientId <- case hasClientId of
        True -> do
            txt <- T.readFile "web/clientId.txt"
            return (ClientId (Just (T.strip txt)))
        False -> return (ClientId Nothing)

    quickHttpServe $ (processBody >> site clientId) <|> site clientId

-- Retrieves the user for the current request.  The request should have an
-- id_token parameter with an id token retrieved from the Google
-- authentication API.  The user is returned if the id token is valid.
getUser :: ClientId -> Snap User
getUser clientId = getParam "id_token" >>= \ case
    Nothing       -> pass
    Just id_token -> do
        let url = "https://www.googleapis.com/oauth2/v1/tokeninfo?id_token=" ++ BC.unpack id_token
        decoded <- fmap decode $ liftIO $ simpleHttp url
        case decoded of
            Nothing -> pass
            Just user -> do
                when (clientId /= ClientId (Just (audience user))) pass
                return user

-- A revised upload policy that allows up to 4 MB of uploaded data in a
-- request.  This is needed to handle uploads of projects including editor
-- history.
codeworldUploadPolicy :: UploadPolicy
codeworldUploadPolicy = setMaximumFormInputSize (2^(22 :: Int)) defaultUploadPolicy

-- Processes the body of a multipart request.
processBody :: Snap ()
processBody = do
    handleMultipart codeworldUploadPolicy (const $ return ())
    return ()

getBuildMode :: Snap BuildMode
getBuildMode = getParam "mode" >>= \ case
    Just "haskell" -> return (BuildMode "haskell")
    _              -> return (BuildMode "codeworld")

site :: ClientId -> Snap ()
site clientId =
    route [
      ("loadProject",   loadProjectHandler clientId),
      ("saveProject",   saveProjectHandler clientId),
      ("deleteProject", deleteProjectHandler clientId),
      ("listProjects",  listProjectsHandler clientId),
      ("compile",       compileHandler),
      ("loadSource",    loadSourceHandler),
      ("run",           runHandler),
      ("runJS",         runHandler),
      ("runMsg",        runMessageHandler),
      ("haskell",       serveFile "web/env.html"),
      ("funblocks",     serveFile "web/blocks.html")
    ] <|>
    serveDirectory "web"

-- A DirectoryConfig that sets the cache-control header to avoid errors when new
-- changes are made to JavaScript.
dirConfig :: DirectoryConfig Snap
dirConfig = defaultDirectoryConfig { preServeHook = disableCache }
  where disableCache _ = modifyRequest (addHeader "Cache-control" "no-cache")

loadProjectHandler :: ClientId -> Snap ()
loadProjectHandler clientId = do
    mode      <- getBuildMode
    user      <- getUser clientId
    liftIO $ ensureUserProjectDir mode (userId user)
    Just name <- getParam "name"
    let projectName = T.decodeUtf8 name
    let projectId = nameToProjectId projectName
    let file = projectRootDir mode </> T.unpack (userId user) </> T.unpack projectId <.> "cw"
    serveFile file

saveProjectHandler :: ClientId -> Snap ()
saveProjectHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    liftIO $ ensureUserProjectDir mode (userId user)
    Just project <- decode . LB.fromStrict . fromJust <$> getParam "project"
    let projectId = nameToProjectId (projectName project)
    let file = projectRootDir mode </> T.unpack (userId user) </> T.unpack projectId <.> "cw"
    liftIO $ LB.writeFile file $ encode project

deleteProjectHandler :: ClientId -> Snap ()
deleteProjectHandler clientId = do
    mode <- getBuildMode
    user      <- getUser clientId
    liftIO $ ensureUserProjectDir mode (userId user)
    Just name <- getParam "name"
    let projectName = T.decodeUtf8 name
    let projectId = nameToProjectId projectName
    let file = projectRootDir mode </> T.unpack (userId user) </> T.unpack projectId <.> "cw"
    liftIO $ removeFile file

listProjectsHandler :: ClientId -> Snap ()
listProjectsHandler clientId = do
    mode <- getBuildMode
    user  <- getUser clientId
    liftIO $ ensureUserProjectDir mode (userId user)
    let projectDir = projectRootDir mode </> T.unpack (userId user)
    projectFiles <- liftIO $ getDirectoryContents projectDir
    projects <- liftIO $ fmap catMaybes $ forM projectFiles $ \f -> do
        let file = projectRootDir mode </> T.unpack (userId user) </> f
        if takeExtension file == ".cw"
            then do
                decode <$> LB.readFile file >>= \ case
                    Nothing -> return Nothing
                    Just project -> do
                        let projId = T.unpack (nameToProjectId (projectName project))
                        if file == projectDir </> projId <.> "cw"
                            then return (Just project)
                            else return Nothing
            else return Nothing
    modifyResponse $ setContentType "application/json"
    writeLBS (encode (map projectName projects))

compileHandler :: Snap ()
compileHandler = do
    mode <- getBuildMode
    Just source <- getParam "source"
    let programId = sourceToProgramId source
    success <- liftIO $ do
        ensureProgramDir mode programId
        B.writeFile (buildRootDir mode </> sourceFile programId) source
        compileIfNeeded mode programId
    when (not success) $ modifyResponse $ setResponseCode 500
    modifyResponse $ setContentType "text/plain"
    writeBS (T.encodeUtf8 programId)

getHashParam :: Snap B.ByteString
getHashParam = do
    Just h <- getParam "hash"
    if "Q" `B.isPrefixOf` h then return ("P" <> B.drop 1 h) else return h

loadSourceHandler :: Snap ()
loadSourceHandler = do
    mode <- getBuildMode
    hash <- getHashParam
    let programId = T.decodeUtf8 hash
    modifyResponse $ setContentType "text/x-haskell"
    serveFile (buildRootDir mode </> sourceFile programId)

runHandler :: Snap ()
runHandler = do
    mode <- getBuildMode
    hash <- getHashParam
    let programId = T.decodeUtf8 hash
    liftIO $ compileIfNeeded mode programId
    modifyResponse $ setContentType "text/javascript"
    serveFile (buildRootDir mode </> targetFile programId)

runMessageHandler :: Snap ()
runMessageHandler = do
    mode <- getBuildMode
    hash <- getHashParam
    let programId = T.decodeUtf8 hash
    liftIO $ compileIfNeeded mode programId
    modifyResponse $ setContentType "text/plain"
    serveFile (buildRootDir mode </> resultFile programId)
