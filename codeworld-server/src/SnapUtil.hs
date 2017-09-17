{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

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

module SnapUtil where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Conduit
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Util.FileUploads

import DataUtil
import Model

newtype ClientId = ClientId (Maybe T.Text) deriving (Eq)

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
#if MIN_VERSION_snap_core(1,0,0)
processBody :: Snap ()
processBody = do
    handleMultipart codeworldUploadPolicy (\_ _ -> return ())
    return ()
#else
processBody :: Snap ()
processBody = do
    handleMultipart codeworldUploadPolicy (\_ -> return ())
    return ()
#endif

getBuildMode :: Snap BuildMode
getBuildMode = getParam "mode" >>= \ case
    Just "haskell" -> return (BuildMode "haskell")
    Just "blocklyXML" -> return (BuildMode "blocklyXML")
    _              -> return (BuildMode "codeworld")

-- A DirectoryConfig that sets the cache-control header to avoid errors when new
-- changes are made to JavaScript.
dirConfig :: DirectoryConfig Snap
dirConfig = defaultDirectoryConfig { preServeHook = disableCache }
  where disableCache _ = modifyRequest (addHeader "Cache-control" "no-cache")

getHashParam :: Bool -> BuildMode -> Snap ProgramId
getHashParam allowDeploy mode = getParam "hash" >>= \case
    Just h -> return (ProgramId (T.decodeUtf8 h))
    Nothing | allowDeploy -> do
        Just dh <- getParam "dhash"
        let deployId = DeployId (T.decodeUtf8 dh)
        liftIO $ resolveDeployId mode deployId
