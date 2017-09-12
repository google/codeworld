{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Collaboration (
    -- routes for simultaneous editing and adding user for collaboration into the project
    collabRoutes,
    ) where

import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           System.FilePath

import CollaborationUtil
import DataUtil
import Model
import SnapUtil

collabRoutes :: ClientId -> [(B.ByteString, Snap ())]
collabRoutes clientId =
    [ ("addToCollaborate",  addToCollaborateHandler clientId)
    , ("collabShare",       collabShareHandler clientId)
    , ("listCurrentOwners", listCurrentOwnersHandler clientId)
    ]

data ParamsGetType = GetFromHash | NotInCommentables deriving (Eq)

getFrequentParams :: ParamsGetType -> ClientId -> Snap (User, BuildMode, FilePath)
getFrequentParams getType clientId = do
    user <- getUser clientId
    mode <- getBuildMode
    case getType of
        NotInCommentables -> do
            Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
            Just name <- getParam "name"
            let projectId = nameToProjectId $ T.decodeUtf8 name
                finalDir = joinPath $ map (dirBase . nameToDirId . T.pack) path'
                file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
            case (length path', path' !! 0) of
                (0, _) -> return (user, mode, file)
                (_, x) | x /= "commentables" -> return (user, mode, file)
        GetFromHash -> do
            Just collabHash <- fmap (CollabId . T.decodeUtf8) <$> getParam "collabHash"
            let collabHashPath = collabHashRootDir mode </> collabHashLink collabHash <.> "cw"
            return (user, mode, collabHashPath)

addToCollaborateHandler :: ClientId -> Snap ()
addToCollaborateHandler clientId = do
    (user, mode, collabHashPath) <- getFrequentParams GetFromHash clientId
    Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    case length path' of
        x | x /= 0 && path' !! 0 == "commentables" -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "Cannot add a project to collaborate with in `commentables` directory."
          | otherwise -> do
            Just userIdent' <- fmap T.decodeUtf8 <$> getParam "userIdent"
            Just name <- getParam "name"
            let pathDir = joinPath $ map (dirBase . nameToDirId . T.pack) path'
                projectId = nameToProjectId . T.decodeUtf8 $ name
                filePath = userProjectDir mode (userId user) </> pathDir </> projectFile projectId
            res <- liftIO $ addForCollaboration mode (userId user) userIdent' name filePath collabHashPath
            case res of
                Left err -> do
                    modifyResponse $ setContentType "text/plain"
                    modifyResponse $ setResponseCode 404
                    writeBS . BC.pack $ err
                Right _ -> return ()

collabShareHandler :: ClientId -> Snap ()
collabShareHandler clientId = do
    (_, _, filePath) <- getFrequentParams NotInCommentables clientId
    collabHashFile <- liftIO $ takeFileName . BC.unpack <$> B.readFile filePath
    modifyResponse $ setContentType "text/plain"
    writeBS . BC.pack . take (length collabHashFile - 3) $ collabHashFile

listCurrentOwnersHandler :: ClientId -> Snap ()
listCurrentOwnersHandler clientId = do
    (_, _, filePath) <- getFrequentParams NotInCommentables clientId
    collabHashPath <- liftIO $ BC.unpack <$> B.readFile filePath
    Just (currentUsers :: [UserDump]) <- liftIO $ decodeStrict <$>
      B.readFile (collabHashPath <.> "users")
    let currentOwners = map (T.unpack . uuserIdent) $ filter (\u -> utype u == "owner") currentUsers
    modifyResponse $ setContentType "application/json"
    writeLBS . encode $ currentOwners
