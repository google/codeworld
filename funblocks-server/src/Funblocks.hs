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

module Funblocks (funblockRoutes, ClientId(..)) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Conduit
import           Snap.Core
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath

import Model
import Util

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

getBuildMode :: Snap BuildMode
getBuildMode = getParam "mode" >>= \ case
    Just "blocklyXML" -> return (BuildMode "blocklyXML")

funblockRoutes :: ClientId -> [(B.ByteString, Snap ())]
funblockRoutes clientId =
    [ ("floadProject",   loadProjectHandler clientId)
    , ("fsaveProject",   saveProjectHandler clientId)
    , ("fdeleteProject", deleteProjectHandler clientId)
    , ("flistFolder",    listFolderHandler clientId)
    , ("fcreateFolder",  createFolderHandler clientId)
    , ("fdeleteFolder",  deleteFolderHandler clientId)
    , ("fshareFolder",   shareFolderHandler clientId)
    , ("fshareContent",  shareContentHandler clientId)
    , ("fmoveProject",   moveProjectHandler clientId)
    , ("fsaveXMLhash",   saveXMLHashHandler)
    , ("floadXML",       loadXMLHandler)
    , ("blocks",        serveFile "web/blocks.html")
    , ("funblocks",     serveFile "web/blocks.html")
    ]

createFolderHandler :: ClientId -> Snap ()
createFolderHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureUserBaseDir mode (userId user) finalDir
    liftIO $ createDirectory $ userProjectDir mode (userId user) </> finalDir
    modifyResponse $ setContentType "text/plain"
    liftIO $ B.writeFile (userProjectDir mode (userId user) </> finalDir </> "dir.info") $ BC.pack $ last path

deleteFolderHandler :: ClientId -> Snap ()
deleteFolderHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureUserDir mode (userId user) finalDir
    let dir = userProjectDir mode (userId user) </> finalDir
    empty <- liftIO $ fmap
        (\ l1 ->
           length l1 == 3 && sort l1 == sort [".", "..", takeFileName dir])
        (getDirectoryContents (takeDirectory dir))
    liftIO $ removeDirectoryIfExists $ if empty then takeDirectory dir else dir

loadProjectHandler :: ClientId -> Snap ()
loadProjectHandler clientId = do
    mode      <- getBuildMode
    user      <- getUser clientId
    Just name <- getParam "name"
    let projectName = T.decodeUtf8 name
    let projectId = nameToProjectId projectName
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureProjectDir mode (userId user) finalDir projectId
    let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
    modifyResponse $ setContentType "application/json"
    serveFile file

saveProjectHandler :: ClientId -> Snap ()
saveProjectHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    Just project <- decode . LB.fromStrict . fromJust <$> getParam "project"
    let projectId = nameToProjectId (projectName project)
    liftIO $ ensureProjectDir mode (userId user) finalDir projectId
    let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
    liftIO $ LB.writeFile file $ encode project

deleteProjectHandler :: ClientId -> Snap ()
deleteProjectHandler clientId = do
    mode      <- getBuildMode
    user      <- getUser clientId
    Just name <- getParam "name"
    let projectName = T.decodeUtf8 name
    let projectId = nameToProjectId projectName
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureProjectDir mode (userId user) finalDir projectId
    let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
    empty <- liftIO $ fmap
        (\ l1 ->
           length l1 == 3 && sort l1 == sort [".", "..", takeFileName file])
        (getDirectoryContents (dropFileName file))
    liftIO $ if empty then removeDirectoryIfExists (dropFileName file)
             else removeFileIfExists file

listFolderHandler :: ClientId -> Snap ()
listFolderHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureUserBaseDir mode (userId user) finalDir
    liftIO $ ensureUserDir mode (userId user) finalDir
    liftIO $ migrateUser $ userProjectDir mode (userId user)
    let projectDir = userProjectDir mode (userId user)
    subHashedDirs <- liftIO $ listDirectoryWithPrefix $ projectDir </> finalDir
    files <- liftIO $ projectFileNames subHashedDirs
    dirs <- liftIO $ projectDirNames subHashedDirs
    modifyResponse $ setContentType "application/json"
    writeLBS (encode (Directory files dirs))

shareFolderHandler :: ClientId -> Snap ()
shareFolderHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    checkSum <- liftIO $ dirToCheckSum $ userProjectDir mode (userId user) </> finalDir
    liftIO $ ensureShareDir mode $ ShareId checkSum
    liftIO $ B.writeFile (shareRootDir mode </> shareLink (ShareId checkSum)) $ BC.pack (userProjectDir mode (userId user) </> finalDir)
    modifyResponse $ setContentType "text/plain"
    writeBS $ T.encodeUtf8 checkSum

shareContentHandler :: ClientId -> Snap ()
shareContentHandler clientId = do
    mode <- getBuildMode
    Just shash <- getParam "shash"
    sharingFolder <- liftIO $ B.readFile (shareRootDir mode </> shareLink (ShareId $ T.decodeUtf8 shash))
    user <- getUser clientId
    Just name <- getParam "name"
    let dirPath = dirBase $ nameToDirId $ T.decodeUtf8 name
    liftIO $ ensureUserBaseDir mode (userId user) dirPath
    liftIO $ copyDirIfExists (BC.unpack sharingFolder) $ userProjectDir mode (userId user) </> dirPath
    liftIO $ B.writeFile (userProjectDir mode (userId user) </> dirPath </> "dir.info") name

moveProjectHandler :: ClientId -> Snap ()
moveProjectHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just moveTo <- fmap (splitDirectories . BC.unpack) <$> getParam "moveTo"
    let moveToDir = joinPath $ map (dirBase . nameToDirId . T.pack) moveTo
    Just moveFrom <- fmap (splitDirectories . BC.unpack) <$> getParam "moveFrom"
    let projectDir = userProjectDir mode (userId user)
    let moveFromDir = projectDir </> joinPath (map (dirBase . nameToDirId . T.pack) moveFrom)
    let parentFrom = if moveFrom == [] then [] else init moveFrom
    Just isFile <- getParam "isFile"
    case (moveTo == moveFrom, moveTo == parentFrom, isFile) of
      (False, _, "true") -> do
        Just name <- getParam "name"
        let projectId = nameToProjectId $ T.decodeUtf8 name
            file = moveFromDir </> projectFile projectId
            toFile = projectDir </> moveToDir </> projectFile projectId
        liftIO $ ensureProjectDir mode (userId user) moveToDir projectId
        liftIO $ copyFile file toFile
        empty <- liftIO $ fmap
            (\ l1 ->
               length l1 == 3 &&
                 sort l1 == sort [".", "..", takeFileName $ projectFile projectId])
            (getDirectoryContents
               (dropFileName $ moveFromDir </> projectFile projectId))
        liftIO $ if empty then removeDirectoryIfExists (dropFileName $ moveFromDir </> projectFile projectId)
                 else removeFileIfExists $ moveFromDir </> projectFile projectId
      (_, False, "false") -> do
        let dirName = last $ splitDirectories moveFromDir
        let dir = moveToDir </> take 3 dirName </> dirName
        liftIO $ ensureUserBaseDir mode (userId user) dir
        liftIO $ copyDirIfExists moveFromDir $ projectDir </> dir
        empty <- liftIO $
          fmap
            (\ l1 ->
               length l1 == 3 &&
                 sort l1 == sort [".", "..", takeFileName moveFromDir])
            (getDirectoryContents (takeDirectory moveFromDir))
        liftIO $ removeDirectoryIfExists $ if empty then takeDirectory moveFromDir else moveFromDir
      (_, _, _) -> return ()

saveXMLHashHandler :: Snap ()
saveXMLHashHandler = do
    mode <- getBuildMode
    unless (mode==BuildMode "blocklyXML") $ modifyResponse $ setResponseCode 500
    Just source <- getParam "source"
    let programId = sourceToProgramId source
    liftIO $ ensureProgramDir mode programId
    liftIO $ B.writeFile (buildRootDir mode </> sourceXML programId) source
    modifyResponse $ setContentType "text/plain"
    writeBS (T.encodeUtf8 (unProgramId programId))

getHashParam :: Bool -> BuildMode -> Snap ProgramId
getHashParam allowDeploy mode = getParam "hash" >>= \case
    Just h -> return (ProgramId (T.decodeUtf8 h))
    Nothing | allowDeploy -> do
        Just dh <- getParam "dhash"
        let deployId = DeployId (T.decodeUtf8 dh)
        liftIO $ resolveDeployId mode deployId

loadXMLHandler :: Snap ()
loadXMLHandler = do
    mode <- getBuildMode
    unless (mode==BuildMode "blocklyXML") $ modifyResponse $ setResponseCode 500
    programId <- getHashParam False mode
    modifyResponse $ setContentType "text/plain"
    serveFile (buildRootDir mode </> sourceXML programId)

getMode :: BuildMode -> String
getMode (BuildMode m) = m
