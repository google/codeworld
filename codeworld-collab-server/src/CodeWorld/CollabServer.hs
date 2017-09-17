{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
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

module CodeWorld.CollabServer
    ( initCollabServer
    , collabServer
    ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad (when)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.OperationalTransformation.Selection as Sel
import qualified Control.OperationalTransformation.Server as OTS
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           DataUtil
import           Model
import           Network.HTTP.Conduit (simpleHttp)
import qualified Network.SocketIO as SIO
import           Snap.Core
import           SnapUtil
import           System.Directory
import           System.FilePath

import CodeWorld.CollabModel

-- Initialize Collab Server

initCollabServer :: IO CollabServerState
initCollabServer = do
    started <- getCurrentTime
    collabProjects <- STM.newTVarIO HM.empty
    return CollabServerState {..}

-- Collaboration requests helpers

getRequestParams :: ClientId -> Snap (User, FilePath)
getRequestParams clientId = do
    user <- getUser clientId
    mode <- getBuildMode
    Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    Just name <- getParam "name"
    let projectId = nameToProjectId $ T.decodeUtf8 name
        finalDir = joinPath $ map (dirBase . nameToDirId . T.pack) path'
        file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
    case (length path', path' !! 0) of
        (0, _) -> return (user, file)
        (_, x) | x /= "commentables" -> return (user, file)

initCollaborationHandler :: CollabServerState -> ClientId -> Snap (Text, Text, CollabId)
initCollaborationHandler state clientId = do
    (user, filePath) <- getRequestParams clientId
    collabHashPath <- liftIO $ BC.unpack <$> B.readFile filePath
    let collabHash = take (length collabHashPath - 3) . takeFileName $ collabHashPath
    Just (currentUsers :: [UserDump]) <- liftIO $ decodeStrict <$>
      B.readFile (collabHashPath <.> "users")
    let userIdent' = uuserIdent $ (filter (\x -> uuserId x == userId user) currentUsers) !! 0
    Just (project :: Project) <- liftIO $ decodeStrict <$>
      B.readFile collabHashPath
    liftIO $ addNewCollaborator state (userId user) userIdent' (projectSource project) $
      CollabId . T.pack $ collabHash
    return ((userId user), userIdent', CollabId . T.pack $ collabHash)

getCollabProject :: CollabServerState -> CollabId -> STM.STM (STM.TVar CollabProject)
getCollabProject state collabHash = do
    fromJust . HM.lookup collabHash <$> STM.readTVar (collabProjects state)

addNewCollaborator :: CollabServerState -> Text -> Text -> Text -> CollabId -> IO ()
addNewCollaborator state userId' userIdent' projectSource collabHash = do
    let collabUser = CollabUserState userId' userIdent' mempty
    STM.atomically $ do
        hm <- STM.readTVar $ collabProjects state
        case HM.lookup collabHash hm of
            Just collabProjectTV -> do
                collabProject <- STM.readTVar collabProjectTV
                case userId' `elem` (map suserId $ users collabProject) of
                    True -> do
                        let collabProject' = collabProject
                                { users = map (\x -> if suserId x == userId'
                                                     then collabUser
                                                     else x) $ users collabProject
                                }
                        collabProjectTV' <- STM.newTVar collabProject'
                        STM.modifyTVar (collabProjects state) $
                          \x -> HM.adjust (\_ -> collabProjectTV') collabHash x
                    False -> do
                        let collabProject' = collabProject
                                { totalUsers = totalUsers collabProject + 1
                                , users      = collabUser : users collabProject
                                }
                        collabProjectTV' <- STM.newTVar collabProject'
                        STM.modifyTVar (collabProjects state) $
                          \x -> HM.adjust (\_ -> collabProjectTV') collabHash x
            Nothing -> do
                let collabProject = CollabProject
                        { totalUsers  = 1
                        , collabKey   = collabHash
                        , collabState = OTS.initialServerState projectSource
                        , users       = [collabUser]
                        }
                collabProjectTV <- STM.newTVar collabProject
                STM.modifyTVar (collabProjects state) $
                  \x -> HM.insert collabHash collabProjectTV x

cleanUp :: CollabServerState -> Text -> STM.TVar CollabProject -> STM.STM ()
cleanUp state userId' collabProjectTV = do
    collabProject <- STM.readTVar collabProjectTV
    case null (filter ((/= userId') . suserId) $ users collabProject) of
        True -> do
            STM.modifyTVar collabProjectTV (\collabProject' -> collabProject'
                                                { totalUsers = 0
                                                , users = []
                                                })
            let collabHash = collabKey collabProject
            STM.modifyTVar (collabProjects state) $ HM.delete collabHash
        False -> do
            STM.modifyTVar collabProjectTV (\collabProject' -> collabProject'
                                                { totalUsers = totalUsers collabProject' - 1
                                                , users = filter ((/= userId') . suserId) $
                                                    users collabProject'
                                                })

-- Collaboration requests handler

collabServer :: CollabServerState -> ClientId -> StateT SIO.RoutingTable (ReaderT SIO.Socket Snap) ()
collabServer state clientId = do
    (userId', userIdent', collabHash) <- liftSnap $ initCollaborationHandler state clientId
    let userHash = hashToId "U" . BC.pack $ (show userId') ++ (show . unCollabId $ collabHash)
    SIO.broadcastJSON "set_name" [toJSON userHash, toJSON userIdent']
    SIO.broadcast "add_user" userIdent'
    SIO.emitJSON "logged_in" []
    currentUsers' <- liftIO . STM.atomically $ do
        collabProjectTV <- getCollabProject state collabHash
        (\x -> map suserIdent $ users x) <$> STM.readTVar collabProjectTV
    collabProjectTV' <- liftIO . STM.atomically $ getCollabProject state collabHash
    OTS.ServerState rev' doc _ <- liftIO $ collabState <$> STM.readTVarIO collabProjectTV'
    SIO.emit "doc" $ object
        [ "str"      .= doc
        , "revision" .= rev'
        , "clients"  .= currentUsers'
        ]

    SIO.on "operation" $ \rev op (sel :: Sel.Selection) -> do
        res <- liftIO . STM.atomically $ do
            collabProjectTV <- getCollabProject state collabHash
            serverState <- collabState <$> STM.readTVar collabProjectTV
            case OTS.applyOperation serverState rev op sel of
                Left err -> return $ Left err
                Right (op', sel', serverState') -> do
                    STM.modifyTVar collabProjectTV (\collabProject ->
                      collabProject { collabState = serverState' })
                    STM.modifyTVar (collabProjects state) $
                      \x -> HM.adjust (\_ -> collabProjectTV) collabHash x
                    return $ Right (op', sel')
        case res of
            Left _ -> return ()
            Right (op', sel') -> do
                SIO.emitJSON "ack" []
                SIO.broadcastJSON "operation" [toJSON userHash, toJSON op', toJSON sel']

    SIO.on "selection" $ \sel -> do
        liftIO . STM.atomically $ do
            collabProjectTV <- getCollabProject state collabHash
            currentUsers <- users <$> STM.readTVar collabProjectTV
            let currentUsers'' = map (\x -> if ((/= userId') . suserId) x
                                               then x
                                               else x{ userSelection = sel }) currentUsers
            STM.modifyTVar collabProjectTV (\collabProject ->
              collabProject { users = currentUsers'' })
            STM.modifyTVar (collabProjects state) $
              \x -> HM.adjust (\_ -> collabProjectTV) collabHash x
        SIO.broadcastJSON "selection" [toJSON userHash, toJSON sel]

    SIO.appendDisconnectHandler $ do
        liftIO . STM.atomically $ do
            collabProjectTV <- getCollabProject state collabHash
            cleanUp state userId' collabProjectTV
        SIO.broadcast "client_left" userHash
        SIO.broadcast "remove_user" userIdent'
