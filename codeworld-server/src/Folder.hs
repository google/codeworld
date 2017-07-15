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

module Folder where

import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.List (sort)
import           Data.Maybe (fromJust)
import           Snap.Core
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath

import DataUtil
import Model
import SnapUtil

folderRoutes :: ClientId -> [(B.ByteString, Snap ())]
folderRoutes clientId =
    [ ("createFolder",  createFolderHandler clientId)
    , ("deleteFolder",  deleteFolderHandler clientId)
    , ("deleteProject", deleteProjectHandler clientId)
    , ("listFolder",    listFolderHandler clientId)
    , ("loadProject",   loadProjectHandler clientId)
    , ("moveProject",   moveProjectHandler clientId)
    , ("shareContent",  shareContentHandler clientId)
    , ("shareFolder",   shareFolderHandler clientId)
    , ("saveProject",   saveProjectHandler clientId)
    ]

getFrequentParams :: Bool -> ClientId -> Snap (User, BuildMode, FilePath, Maybe ProjectId)
getFrequentParams file clientId = do
    user <- getUser clientId
    mode <- getBuildMode
    Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let finalDir = joinPath $ map (dirBase . nameToDirId . T.pack) path'
    case file of
      True -> do
        Just name <- getParam "name"
        let projectId = nameToProjectId $ T.decodeUtf8 name
        return (user, mode, finalDir, Just projectId)
      False -> return (user, mode, finalDir, Nothing)

createFolderHandler :: ClientId -> Snap ()
createFolderHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    liftIO $ ensureUserBaseDir mode (userId user) finalDir
    liftIO $ createDirectory $ userProjectDir mode (userId user) </> finalDir
    liftIO $ B.writeFile (userProjectDir mode (userId user) </> finalDir </> "dir.info") $
      BC.pack $ last path'

deleteFolderHandler :: ClientId -> Snap ()
deleteFolderHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    let dir' = userProjectDir mode (userId user) </> finalDir
    empty <- liftIO $ fmap
        (\ l1 ->
           length l1 == 3 && sort l1 == sort [".", "..", takeFileName dir'])
        (getDirectoryContents (takeDirectory dir'))
    liftIO $ removeDirectoryIfExists $ if empty then takeDirectory dir' else dir'

deleteProjectHandler :: ClientId -> Snap ()
deleteProjectHandler clientId = do
    (user, mode, finalDir, Just projectId) <- getFrequentParams True clientId
    let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
    liftIO $ removeFileIfExists file
    liftIO $ removeDirectoryIfExists $ file <.> "comments"
    empty <- liftIO $ fmap
        (\ l1 ->
           length l1 == 2 && sort l1 == sort [".", ".."])
        (getDirectoryContents (dropFileName file))
    liftIO $ if empty then removeDirectoryIfExists (dropFileName file)
             else return ()

listFolderHandler :: ClientId -> Snap ()
listFolderHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    liftIO $ migrateUser $ userProjectDir mode (userId user)
    let projectDir = userProjectDir mode (userId user)
    subHashedDirs <- liftIO $ listDirectoryWithPrefix $ projectDir </> finalDir
    files' <- liftIO $ projectFileNames subHashedDirs
    dirs' <- liftIO $ projectDirNames subHashedDirs
    modifyResponse $ setContentType "application/json"
    writeLBS (encode (Directory files' dirs'))

loadProjectHandler :: ClientId -> Snap ()
loadProjectHandler clientId = do
    (user, mode, finalDir, Just projectId) <- getFrequentParams True clientId
    let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
    modifyResponse $ setContentType "application/json"
    serveFile file

moveProjectHandler :: ClientId -> Snap ()
moveProjectHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just moveTo <- fmap (splitDirectories . BC.unpack) <$> getParam "moveTo"
    let moveToDir = joinPath $ map (dirBase . nameToDirId . T.pack) moveTo
    Just moveFrom <- fmap (splitDirectories . BC.unpack) <$> getParam "moveFrom"
    let projectDir = userProjectDir mode (userId user)
    let moveFromDir = projectDir </> joinPath (map (dirBase . nameToDirId . T.pack) moveFrom)
    Just isFile <- getParam "isFile"
    case (moveTo == moveFrom, isFile) of
      (False, "true") -> do
        Just name <- getParam "name"
        let projectId = nameToProjectId $ T.decodeUtf8 name
        liftIO $ ensureProjectDir mode (userId user) moveToDir projectId
        liftIO $ copyDirIfExists (dropFileName $ moveFromDir </> projectFile projectId)
                                 (dropFileName $ projectDir </> moveToDir </> projectFile projectId)
        let file = moveFromDir </> projectFile projectId
        liftIO $ removeFileIfExists file
        liftIO $ removeDirectoryIfExists $ file <.> "comments"
        empty <- liftIO $ fmap
            (\ l1 ->
               length l1 == 2 &&
                 sort l1 == sort [".", ".."])
            (getDirectoryContents
               (dropFileName file))
        liftIO $ if empty then removeDirectoryIfExists (dropFileName file)
                 else return ()
      (False, "false") -> do
        let dirName = last $ splitDirectories moveFromDir
        let dir' = moveToDir </> take 3 dirName </> dirName
        liftIO $ ensureUserBaseDir mode (userId user) dir'
        liftIO $ copyDirIfExists moveFromDir $ projectDir </> dir'
        empty <- liftIO $
          fmap
            (\ l1 ->
               length l1 == 3 &&
                 sort l1 == sort [".", "..", takeFileName moveFromDir])
            (getDirectoryContents (takeDirectory moveFromDir))
        liftIO $ removeDirectoryIfExists $ if empty then takeDirectory moveFromDir else moveFromDir
      (_, _) -> return ()

shareContentHandler :: ClientId -> Snap ()
shareContentHandler clientId = do
    mode <- getBuildMode
    Just shash <- getParam "shash"
    sharingFolder <- liftIO $
      B.readFile (shareRootDir mode </> shareLink (ShareId $ T.decodeUtf8 shash))
    user <- getUser clientId
    Just name <- getParam "name"
    let dirPath = dirBase $ nameToDirId $ T.decodeUtf8 name
    liftIO $ ensureUserBaseDir mode (userId user) dirPath
    liftIO $ copyDirIfExists (BC.unpack sharingFolder) $
      userProjectDir mode (userId user) </> dirPath
    liftIO $ B.writeFile (userProjectDir mode (userId user) </> dirPath </> "dir.info") name

shareFolderHandler :: ClientId -> Snap ()
shareFolderHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    checkSum <- liftIO $ dirToCheckSum $ userProjectDir mode (userId user) </> finalDir
    liftIO $ ensureShareDir mode $ ShareId checkSum
    liftIO $ B.writeFile (shareRootDir mode </> shareLink (ShareId checkSum)) $
      BC.pack (userProjectDir mode (userId user) </> finalDir)
    modifyResponse $ setContentType "text/plain"
    writeBS $ T.encodeUtf8 checkSum

saveProjectHandler :: ClientId -> Snap ()
saveProjectHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    Just project <- decode . LB.fromStrict . fromJust <$> getParam "project"
    let projectId = nameToProjectId (projectName project)
    liftIO $ ensureProjectDir mode (userId user) finalDir projectId
    let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
    liftIO $ LB.writeFile file $ encode project
