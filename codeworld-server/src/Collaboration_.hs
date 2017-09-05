{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

module Collaboration_ where

import qualified Control.Concurrent.STM as STM
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
import qualified Network.SocketIO as SIO
import           Snap.Core
import           System.FilePath

import CollaborationUtil
import DataUtil
import Model
import SnapUtil

collabRoutes :: Snap () -> ClientId -> [(B.ByteString, Snap ())]
collabRoutes socketIOHandler clientId =
    [ ("addToCollaborate",  addToCollaborateHandler clientId)
    , ("collabShare",       collabShareHandler clientId)
    , ("listCurrentOwners", listCurrentOwnersHandler clientId)
    , ("socket.io", socketIOHandler)
    ]

getFrequentParams :: Int -> ClientId -> Snap (User, BuildMode, FilePath)
getFrequentParams getType clientId = do
    user <- getUser clientId
    mode <- getBuildMode
    case getType of
        1 -> do
            Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
            Just name <- getParam "name"
            let projectId = nameToProjectId $ T.decodeUtf8 name
                finalDir = joinPath $ map (dirBase . nameToDirId . T.pack) path'
                file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
            case length path' of
                0 -> return (user, mode, file)
                _ -> case path' !! 0 of
                         x | x /= "commentables" -> return (user, mode, file)
        _ -> do
            Just collabHash <- fmap (CollabId . T.decodeUtf8) <$> getParam "collabHash"
            let collabHashPath = collabHashRootDir mode </> collabHashLink collabHash <.> "cw"
            return (user, mode, collabHashPath)

addToCollaborateHandler :: ClientId -> Snap ()
addToCollaborateHandler clientId = do
    (user, mode, collabHashPath) <- getFrequentParams 2 clientId
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
    (_, _, filePath) <- getFrequentParams 1 clientId
    collabHashFile <- liftIO $ takeFileName . BC.unpack <$> B.readFile filePath
    modifyResponse $ setContentType "text/plain"
    writeBS . BC.pack . take (length collabHashFile - 3) $ collabHashFile

listCurrentOwnersHandler :: ClientId -> Snap ()
listCurrentOwnersHandler clientId = do
    (_, _, filePath) <- getFrequentParams 1 clientId
    collabHashPath <- liftIO $ BC.unpack <$> B.readFile filePath
    Just (currentUsers :: [UserDump]) <- liftIO $ decodeStrict <$>
      B.readFile (collabHashPath <.> "users")
    let currentOwners = map (T.unpack . uuserIdent) $ filter (\u -> utype u == "owner") currentUsers
    modifyResponse $ setContentType "application/json"
    writeLBS . encode $ currentOwners

-- Simultaneous Coding Handlers

initCollabServer :: IO CollabServerState
initCollabServer = do
    started <- getCurrentTime
    collabProjects <- STM.newTVarIO HM.empty
    return CollabServerState{..}

initCollaborationHandler :: CollabServerState -> ClientId -> Snap (Text, Text, CollabId)
initCollaborationHandler state clientId = do
    (user, _, filePath) <- getFrequentParams 1 clientId
    collabHashPath <- liftIO $ BC.unpack <$> B.readFile filePath
    let collabHash = take (length collabHashPath - 3) . takeFileName $ collabHashPath
    Just (currentUsers :: [UserDump]) <- liftIO $ decodeStrict <$>
      B.readFile (collabHashPath <.> "users")
    let userIdent' = uuserIdent $ (filter (\x -> uuserId x == userId user) currentUsers) !! 0
    Just (project :: Project) <- liftIO $ decodeStrict <$>
      B.readFile collabHashPath
    liftIO $ addNewCollaborator state (userId user) userIdent' project $ CollabId . T.pack $ collabHash
    return ((userId user), userIdent', CollabId . T.pack $ collabHash)

addNewCollaborator :: CollabServerState -> Text -> Text -> Project -> CollabId -> IO ()
addNewCollaborator state userId' userIdent' project collabHash = do
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
                        STM.modifyTVar (collabProjects state) $ \x -> HM.adjust (\_ -> collabProjectTV') collabHash x
                    False -> do
                        let collabProject' = collabProject
                                        { totalUsers = totalUsers collabProject + 1
                                        , users      = collabUser : users collabProject
                                        }
                        collabProjectTV' <- STM.newTVar collabProject'
                        STM.modifyTVar (collabProjects state) $ \x -> HM.adjust (\_ -> collabProjectTV') collabHash x
            Nothing -> do
                let collabProject = CollabProject
                                { totalUsers  = 1
                                , collabKey   = collabHash
                                , collabState = OTS.initialServerState (projectSource project)
                                , users       = [collabUser]
                                }
                collabProjectTV <- STM.newTVar collabProject
                STM.modifyTVar (collabProjects state) $ \x -> HM.insert collabHash collabProjectTV x

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
                                                        , users = filter ((/= userId') . suserId) $ users collabProject'
                                                        })

getCollabProject :: CollabServerState -> CollabId -> STM.STM (STM.TVar CollabProject)
getCollabProject state collabHash = do
    hm <- STM.readTVar $ collabProjects state
    return $ fromJust . HM.lookup collabHash $ hm

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
                    STM.modifyTVar collabProjectTV (\collabProject -> collabProject { collabState = serverState' })
                    STM.modifyTVar (collabProjects state) $ \x -> HM.adjust (\_ -> collabProjectTV) collabHash x
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
            STM.modifyTVar collabProjectTV (\collabProject -> collabProject { users = currentUsers'' })
            STM.modifyTVar (collabProjects state) $ \x -> HM.adjust (\_ -> collabProjectTV) collabHash x
        SIO.broadcastJSON "selection" [toJSON userHash, toJSON sel]

    SIO.appendDisconnectHandler $ do
        liftIO . STM.atomically $ do
            collabProjectTV <- getCollabProject state collabHash
            cleanUp state userId' collabProjectTV
        SIO.broadcast "client_left" userHash
        SIO.broadcast "remove_user" userIdent'
