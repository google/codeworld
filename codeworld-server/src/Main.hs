{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2014 Google Inc. All rights reserved.

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

main :: IO ()
main = do
    hasClientId <- doesFileExist "web/clientId.txt"
    when (not hasClientId) $ do
        putStrLn "WARNING: Missing web/clientId.txt"
        putStrLn "User logins will not function properly!"

    hasAutocomplete <- doesFileExist "web/autocomplete.txt"
    when (not hasAutocomplete) $ do
        putStrLn "WARNING: Missing web/autocomplete.txt"
        putStrLn "Autocomplete will not function properly!"

    generateBaseBundle
    quickHttpServe $ (processBody >> site) <|> site

-- Retrieves the user for the current request.  The request should have an
-- id_token parameter with an id token retrieved from the Google
-- authentication API.  The user is returned if the id token is valid.
getUser :: Snap User
getUser = getParam "id_token" >>= \ case
    Nothing       -> pass
    Just id_token -> maybe pass return =<< (fmap decode $ liftIO $ simpleHttp $
        "https://www.googleapis.com/oauth2/v1/tokeninfo?id_token=" ++ BC.unpack id_token)

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

site :: Snap ()
site =
    route [
      ("loadProject",   loadProjectHandler),
      ("saveProject",   saveProjectHandler),
      ("deleteProject", deleteProjectHandler),
      ("compile",       compileHandler),
      ("run",           runHandler),
      ("runJS",         runHandler),
      ("listExamples",  listExamplesHandler),
      ("listProjects",  listProjectsHandler)
    ] <|>
    dir "user" (serveDirectory "user") <|>
    serveDirectory "web"

loadProjectHandler :: Snap ()
loadProjectHandler = do
    user      <- getUser
    Just name <- getParam "name"
    let hash = T.decodeUtf8 (getHash name)
    let fname = "projects" </> T.unpack (hash <> "." <> userId user <> ".cw")
    serveFile fname

saveProjectHandler :: Snap ()
saveProjectHandler = do
    user <- getUser
    Just project <- decode . LB.fromStrict . fromJust <$> getParam "project"
    let hash = getHash (T.encodeUtf8 (projectName project))
    let fname = T.decodeUtf8 hash <> "." <> userId user <> ".cw"
    liftIO $ LB.writeFile ("projects" </> T.unpack fname) $ encode project

deleteProjectHandler :: Snap ()
deleteProjectHandler = do
    user      <- getUser
    Just name <- getParam "name"
    let hash = T.decodeUtf8 (getHash name)
    let fname = "projects" </> T.unpack (hash <> "." <> userId user <> ".cw")
    liftIO $ removeFile fname

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

listProjectsHandler :: Snap ()
listProjectsHandler = do
    user  <- getUser
    projects <- liftIO $ do
        let ext = T.unpack $ "." <> userId user <> ".cw"
        let base = "projects"
        files <- getFilesByExt ext base
        mapM (fmap (fromJust . decode) . LB.readFile . (base </>)) files :: IO [Project]
    modifyResponse $ setContentType "application/json"
    writeLBS (encode (map projectName projects))
