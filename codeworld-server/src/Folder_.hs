{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
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

module Folder_ where

import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.List
import           Data.Maybe (fromJust)
import           Snap.Core
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath

import CommentUtil
import DataUtil
import Model
import SnapUtil

folderRoutes :: ClientId -> [(B.ByteString, Snap ())]
folderRoutes clientId =
    [ ("copyProject",   copyProjectHandler clientId)
    , ("createFolder",  createFolderHandler clientId)
    , ("deleteFolder",  deleteFolderHandler clientId)
    , ("deleteProject", deleteProjectHandler clientId)
    , ("listFolder",    listFolderHandler clientId)
    , ("loadProject",   loadProjectHandler clientId)
    , ("moveProject",   moveProjectHandler clientId)
    , ("newProject",    newProjectHandler clientId)
    , ("shareContent",  shareContentHandler clientId)
    , ("shareFolder",   shareFolderHandler clientId)
    , ("saveProject",   saveProjectHandler clientId)
    ]

getFrequentParams :: Bool -> ClientId -> Snap (User, BuildMode, FilePath, Maybe ProjectId)
getFrequentParams file clientId = do
    user <- getUser clientId
    mode <- getBuildMode
    Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let finalDir = case length path' of
                     0 -> ""
                     _ | path' !! 0 == "commentables" -> "commentables" </> (joinPath $
                           map (dirBase . nameToDirId . T.pack) $ tail path')
                       | otherwise -> joinPath $ map (dirBase . nameToDirId . T.pack) path'
    case file of
      True -> do
        Just name <- getParam "name"
        let projectId = nameToProjectId $ T.decodeUtf8 name
        return (user, mode, finalDir, Just projectId)
      False -> return (user, mode, finalDir, Nothing)

copyProjectHandler :: ClientId -> Snap ()
copyProjectHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just copyTo <- fmap (splitDirectories . BC.unpack) <$> getParam "copyTo"
    Just copyFrom <- fmap (splitDirectories . BC.unpack) <$> getParam "copyFrom"
    let projectDir = userProjectDir mode (userId user)
        toType = (length copyTo > 0) && copyTo !! 0 == "commentables"
        fromType = (length copyFrom > 0) && copyFrom !! 0 == "commentables"
        copyToDir = case toType of
                      True -> "commentables" </> (joinPath $
                                map (dirBase . nameToDirId . T.pack) $ tail copyTo)
                      False -> joinPath $ map (dirBase . nameToDirId . T.pack) copyTo
        copyFromDir = case fromType of
                        True -> "commentables" </> (joinPath $
                                  map (dirBase . nameToDirId . T.pack) $ tail copyFrom)
                        False -> joinPath $ map (dirBase . nameToDirId . T.pack) copyFrom
    case toType of
      True -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $ "Cannot Copy Something Into `commentables` Directory"
      False -> do
        Just isFile <- getParam "isFile"
        Just name <- fmap BC.unpack <$> getParam "name"
        Just fromName <- fmap BC.unpack <$> getParam "fromName"
        Just (emptyPH :: Value) <- decode . LB.fromStrict . fromJust <$> getParam "empty"
        let name' = if name == "commentables" then "commentables'" else name
            fromName' = if fromName == "commentables" then "commentables'" else fromName
        case (copyTo == copyFrom && fromName' == name', isFile) of
          (False, "true") -> do
            let projectId = nameToProjectId . T.pack $ name'
                fromProjectId = nameToProjectId . T.pack $ fromName'
                toFile = projectDir </> copyToDir </> projectFile projectId
            case fromType of
              True -> liftIO $ do
                let fromFile = projectDir </> copyFromDir </> commentProjectLink fromProjectId
                copyFileFromCommentables mode (userId user)
                  fromFile toFile (T.pack name') emptyPH
              False -> liftIO $ do
                let fromFile = projectDir </> copyFromDir </> projectFile projectId
                copyFileFromSelf mode (userId user) fromFile toFile $ T.pack name'
          (False, "false") -> do
            let toDir = copyToDir </> (dirBase . nameToDirId . T.pack $ name')
                fromDir = copyFromDir </> (dirBase . nameToDirId . T.pack $ fromName')
            _ <- liftIO $ deleteFolderWithComments mode (userId user) toDir
            case fromType of
              True -> liftIO $ do
                copyFolderFromCommentables mode (userId user) (projectDir </> fromDir)
                  (projectDir </> toDir) (T.pack name') emptyPH
              False -> liftIO $ do
                copyFolderFromSelf mode (userId user) (projectDir </> fromDir)
                  (projectDir </> toDir) $ T.pack name'
          (_, _) -> return ()

createFolderHandler :: ClientId -> Snap ()
createFolderHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    case finalDir == "commentables" of
      True -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $
          "`commentables` Hash Directory Is Forbidden In Root Folder For User Use"
      False -> do
        Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
        dirBool <- liftIO $ doesDirectoryExist finalDir
        case dirBool of
          True -> do
            res <- liftIO $ deleteFolderWithComments mode (userId user) finalDir
            case res of
              Left err -> do
                modifyResponse $ setContentType "text/plain"
                modifyResponse $ setResponseCode 500
                writeBS . BC.pack $ err
              Right _ -> liftIO $ do
                createNewFolder mode (userId user) finalDir (last path')
          False -> liftIO $ do
            createNewFolder mode (userId user) finalDir (last path')

deleteFolderHandler :: ClientId -> Snap ()
deleteFolderHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    res <- liftIO $ deleteFolderWithComments mode (userId user) finalDir
    case res of
      Left err -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $ err
      Right _ -> return ()

deleteProjectHandler :: ClientId -> Snap ()
deleteProjectHandler clientId = do
    (user, mode, finalDir, Just projectId) <- getFrequentParams True clientId
    case length (splitDirectories finalDir) of
      x | (x /= 0) && ((splitDirectories finalDir) !! 0 == "commentables") -> do
           let file = userProjectDir mode (userId user) </>
                        finalDir </> commentProjectLink projectId
           liftIO $ removeUserFromComments (userId user) file
        | otherwise -> do
           let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
           liftIO $ cleanCommentPaths mode (file <.> "comments")

listFolderHandler :: ClientId -> Snap ()
listFolderHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    liftIO $ migrateUser $ userProjectDir mode (userId user)
    liftIO $ ensureSharedCommentsDir mode (userId user)
    let projectDir = userProjectDir mode (userId user)
    subHashedDirs <- liftIO $ listDirectoryWithPrefix $ projectDir </> finalDir
    let subHashedDirs' = case finalDir == "" of
                           True -> delete (projectDir </> "commentables") subHashedDirs
                           False -> subHashedDirs
    files' <- liftIO $ projectFileNames subHashedDirs'
    dirs' <- liftIO $ projectDirNames subHashedDirs'
    modifyResponse $ setContentType "application/json"
    case finalDir == "" of
      True -> writeLBS (encode (Directory files' ("commentables" : dirs')))
      False -> writeLBS (encode (Directory files' dirs'))

loadProjectHandler :: ClientId -> Snap ()
loadProjectHandler clientId = do
    (user, mode, finalDir, Just projectId) <- getFrequentParams True clientId
    case length (splitDirectories finalDir) of
      x | (x /= 0) && ((splitDirectories finalDir) !! 0 == "commentables") -> do
           modifyResponse $ setContentType "text/plain"
           modifyResponse $ setResponseCode 500
           writeBS . BC.pack $ "Wrong Route To View A Source In `commentables` Directory"
        | otherwise -> do
           let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
           modifyResponse $ setContentType "application/json"
           serveFile file

moveProjectHandler :: ClientId -> Snap ()
moveProjectHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just moveTo <- fmap (splitDirectories . BC.unpack) <$> getParam "moveTo"
    Just moveFrom <- fmap (splitDirectories . BC.unpack) <$> getParam "moveFrom"
    let projectDir = userProjectDir mode (userId user)
        toType = (length moveTo > 0) && moveTo !! 0 == "commentables"
        fromType = (length moveFrom > 0) && moveFrom !! 0 == "commentables"
        moveToDir = case toType of
                      True -> "commentables" </> (joinPath $
                                map (dirBase . nameToDirId . T.pack) $ tail moveTo)
                      False -> joinPath $ map (dirBase . nameToDirId . T.pack) moveTo
        moveFromDir = case fromType of
                        True -> "commentables" </> (joinPath $
                                  map (dirBase . nameToDirId . T.pack) $ tail moveFrom)
                        False -> joinPath $ map (dirBase . nameToDirId . T.pack) moveFrom
    case (toType && fromType) || (not $ toType || fromType) of
      True -> do
        Just isFile <- getParam "isFile"
        Just name <- fmap BC.unpack <$> getParam "name"
        Just fromName <- fmap BC.unpack <$> getParam "fromName"
        let name' = if name == "commentables" then "commentables'" else name
            fromName' = if fromName == "commentables"  then "commentables'" else fromName
        case (moveTo == moveFrom && fromName' == name', isFile) of
          (False, "true") -> do
            let projectId = nameToProjectId . T.pack $ name'
                fromProjectId = nameToProjectId . T.pack $ fromName'
            case toType of
              True -> liftIO $ do
                let fromFile = projectDir </> moveFromDir </> commentProjectLink fromProjectId
                    toFile = projectDir </> moveToDir </> commentProjectLink projectId
                moveFileFromCommentables (userId user) fromFile toFile $ T.pack name'
              False -> liftIO $ do
                let fromFile = projectDir </> moveFromDir </> projectFile fromProjectId
                    toFile = projectDir </> moveToDir </> projectFile projectId
                moveFileFromSelf mode (userId user) fromFile toFile $ T.pack name'
          (False, "false") -> do
            let toDir = moveToDir </> (dirBase . nameToDirId . T.pack $ name')
                fromDir = moveFromDir </> (dirBase . nameToDirId . T.pack $ fromName')
            _ <- liftIO $ deleteFolderWithComments mode (userId user) toDir
            case toType of
              True -> liftIO $ do
                moveFolderFromCommentables mode (userId user) (projectDir </> fromDir)
                  (projectDir </> toDir) $ T.pack name'
              False -> liftIO $ do
                moveFolderFromSelf mode (userId user) (projectDir </> fromDir)
                  (projectDir </> toDir) $ T.pack name'
          (_, _) -> return ()
      False -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $ "Cannot Move From `commentables` to Normal and vice-versa"

newProjectHandler :: ClientId -> Snap ()
newProjectHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    case length (splitDirectories finalDir) of
      x | (x /= 0) && ((splitDirectories finalDir) !! 0 == "commentables") -> do
           modifyResponse $ setContentType "text/plain"
           modifyResponse $ setResponseCode 500
           writeBS . BC.pack $ "`commentables` Directory Does Not Allows New Projects"
        | otherwise -> do
           Just (project :: Project) <- decode . LB.fromStrict . fromJust <$> getParam "project"
           let projectId = nameToProjectId (projectName project)
               file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
           liftIO $ do
               cleanCommentPaths mode $ file <.> "comments"
               ensureProjectDir mode (userId user) finalDir projectId
               LB.writeFile file $ encode project
               addSelf mode (userId user) "Anonymous Owner" $ file <.> "comments"

shareContentHandler :: ClientId -> Snap ()
shareContentHandler clientId = do
    mode <- getBuildMode
    Just shash <- getParam "shash"
    sharingFolder <- liftIO $
      B.readFile (shareRootDir mode </> shareLink (ShareId $ T.decodeUtf8 shash))
    user <- getUser clientId
    Just name <- getParam "name"
    let dirPath = dirBase $ nameToDirId $ T.decodeUtf8 name
    liftIO $ do
        ensureUserBaseDir mode (userId user) dirPath
        copyDirIfExists (BC.unpack sharingFolder) $
          userProjectDir mode (userId user) </> dirPath
        B.writeFile (userProjectDir mode (userId user) </> dirPath </> "dir.info") name

shareFolderHandler :: ClientId -> Snap ()
shareFolderHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    case length (splitDirectories finalDir) of
      x | (x /= 0) && ((splitDirectories finalDir) !! 0 == "commentables") -> do
           modifyResponse $ setContentType "text/plain"
           modifyResponse $ setResponseCode 500
           writeBS . BC.pack $ "Contents In `commentables` Directory Cannot Be Shared"
        | otherwise -> do
           checkSum <- liftIO $ dirToCheckSum $ userProjectDir mode (userId user) </> finalDir
           liftIO $ ensureShareDir mode $ ShareId checkSum
           liftIO $ B.writeFile (shareRootDir mode </> shareLink (ShareId checkSum)) $
             BC.pack (userProjectDir mode (userId user) </> finalDir)
           modifyResponse $ setContentType "text/plain"
           writeBS $ T.encodeUtf8 checkSum

saveProjectHandler :: ClientId -> Snap ()
saveProjectHandler clientId = do
    (user, mode, finalDir, _) <- getFrequentParams False clientId
    case length (splitDirectories finalDir) of
      x | (x /= 0) && ((splitDirectories finalDir) !! 0 == "commentables") -> do
           modifyResponse $ setContentType "text/plain"
           modifyResponse $ setResponseCode 500
           writeBS . BC.pack $ "`commentables` Directory Does Not Allows Editing Projects"
        | otherwise -> do
           Just (project :: Project) <- decode . LB.fromStrict . fromJust <$> getParam "project"
           let projectId = nameToProjectId (projectName project)
               file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
           liftIO $ do
            -- no need to ensure a project file as
            -- constrained to create a new project before editing.
               LB.writeFile file $ encode project
               createNewVersionIfReq (projectSource project) $ file <.> "comments"
