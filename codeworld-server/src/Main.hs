{-# LANGUAGE LambdaCase #-}
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
import Paths
import Util

newtype ClientId = ClientId (Maybe T.Text) deriving (Eq)

main :: IO ()
main = do
    createDirectoryIfMissing True buildDir
    createDirectoryIfMissing True projectDir
    generateBaseBundle

    hasClientId <- doesFileExist "web/clientId.txt"
    when (not hasClientId) $ do
        putStrLn "WARNING: Missing web/clientId.txt"
        putStrLn "User logins will not function properly!"

    clientId <- case hasClientId of
        True -> do
            txt <- T.readFile "web/clientId.txt"
            return (ClientId (Just (T.strip txt)))
        False -> return (ClientId Nothing)

    hasAutocomplete <- doesFileExist "web/autocomplete.txt"
    when (not hasAutocomplete) $ do
        putStrLn "WARNING: Missing web/autocomplete.txt"
        putStrLn "Autocomplete will not function properly!"

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

site :: ClientId -> Snap ()
site clientId =
    route [
      ("loadProject",   loadProjectHandler clientId),
      ("saveProject",   saveProjectHandler clientId),
      ("deleteProject", deleteProjectHandler clientId),
      ("listProjects",  listProjectsHandler clientId),
      ("compile",       compileHandler),
      ("run",           runHandler),
      ("runJS",         runHandler),
      ("listExamples",  listExamplesHandler)
    ] <|>
    dir "user" (serveDirectoryWith dirConfig buildDir) <|>
    serveDirectory "web"

-- A DirectoryConfig that sets the cache-control header to avoid errors when new
-- changes are made to JavaScript.
dirConfig :: DirectoryConfig Snap
dirConfig = defaultDirectoryConfig { preServeHook = disableCache }
  where disableCache _ = modifyRequest (addHeader "Cache-control" "no-cache")

loadProjectHandler :: ClientId -> Snap ()
loadProjectHandler clientId = do
    user      <- getUser clientId
    Just name <- getParam "name"
    let hash = T.decodeUtf8 (getHash name)
    let fname = projectDir </> T.unpack (hash <> "." <> userId user <> ".cw")
    serveFile fname

saveProjectHandler :: ClientId -> Snap ()
saveProjectHandler clientId = do
    user <- getUser clientId
    Just project <- decode . LB.fromStrict . fromJust <$> getParam "project"
    let hash = getHash (T.encodeUtf8 (projectName project))
    let fname = T.decodeUtf8 hash <> "." <> userId user <> ".cw"
    liftIO $ LB.writeFile (projectDir </> T.unpack fname) $ encode project

deleteProjectHandler :: ClientId -> Snap ()
deleteProjectHandler clientId = do
    user      <- getUser clientId
    Just name <- getParam "name"
    let hash = T.decodeUtf8 (getHash name)
    let fname = projectDir </> T.unpack (hash <> "." <> userId user <> ".cw")
    liftIO $ removeFile fname

listProjectsHandler :: ClientId -> Snap ()
listProjectsHandler clientId = do
    user  <- getUser clientId
    projects <- liftIO $ do
        let ext = T.unpack $ "." <> userId user <> ".cw"
        files <- getFilesByExt ext projectDir
        mapM (fmap (fromJust . decode) . LB.readFile . (projectDir </>)) files :: IO [Project]
    modifyResponse $ setContentType "application/json"
    writeLBS (encode (map projectName projects))

compileHandler :: Snap ()
compileHandler = do
    Just source <- getParam "source"
    let hashed = BC.cons 'P' (getHash source)
    success <- liftIO $ do
        B.writeFile (sourceFile hashed) source
        compileIfNeeded hashed
    when (not success) $ modifyResponse $ setResponseCode 500
    modifyResponse $ setContentType "text/plain"
    writeBS hashed

runHandler :: Snap ()
runHandler = do
    Just hashed <- getParam "hash"
    liftIO $ compileIfNeeded hashed
    serveFile (targetFile hashed)

listExamplesHandler :: Snap ()
listExamplesHandler = do
    files <- liftIO $ getFilesByExt ".hs" "web/examples"
    modifyResponse $ setContentType "application/json"
    writeLBS (encode files)
