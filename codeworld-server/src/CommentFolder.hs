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

module CommentFolder where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import           Data.List
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.Directory
import           System.FilePath

import CollaborationUtil
import DataUtil
import Model

cleanCommentPaths :: BuildMode -> Text -> FilePath -> IO ()
cleanCommentPaths mode userId' file = do
    fileBool <- doesFileExist file
    case fileBool of
        True -> removeProjectIfExists mode userId' file
        False -> return ()

deleteFolderWithComments :: BuildMode -> Text -> FilePath -> IO (Either String ())
deleteFolderWithComments mode userId' finalDir = do
    let dir' = userProjectDir mode userId' </> finalDir
    dirBool <- doesDirectoryExist dir'
    case dirBool of
        True -> do
            case finalDir == "commentables" of
                True -> return $ Left "`commentables` Directory Cannot Be Deleted"
                False -> do
                    allFilePaths <- getFilesRecursive dir'
                    case length (splitDirectories finalDir) of
                        x | x == 0 -> return $ Left "Root Directory Cannot Be Deleted"
                          | (x /= 0) && ((splitDirectories finalDir) !! 0 == "commentables") -> do
                            mapM_ (removeUserFromComments userId') allFilePaths
                            removeDirectoryIfExists dir'
                            cleanBaseDirectory dir'
                            return $ Right ()
                          | otherwise -> do
                            mapM_ (cleanCommentPaths mode userId') allFilePaths
                            removeDirectoryIfExists dir'
                            cleanBaseDirectory dir'
                            return $ Right ()
        False -> return $ Left "Directory Does Not Exists"

removeUserFromComments :: Text -> FilePath -> IO ()
removeUserFromComments userId' userPath = do
    fileBool <- doesFileExist userPath
    case fileBool of
        True -> do
            commentHashFile <- BC.unpack <$> B.readFile userPath
            commentFolder <- BC.unpack <$> B.readFile commentHashFile
            Just (currentUsers :: [UserDump]) <- decodeStrict <$>
              B.readFile (commentHashFile <.> "users")
            let currentUserIds = map uuserId currentUsers
                currentUser = currentUsers !! (fromJust $ userId' `elemIndex` currentUserIds)
            removeFileIfExists $ commentFolder <.> "users" </> T.unpack (uuserIdent currentUser)
            B.writeFile (commentHashFile <.> "users") $
              LB.toStrict $ encode (delete currentUser currentUsers)
            removeFileIfExists userPath
            removeFileIfExists $ userPath <.> "info"
            cleanBaseDirectory userPath
        False -> return ()

copyFileFromCommentables :: BuildMode -> Text -> Text -> FilePath -> FilePath -> ByteString -> Value -> IO ()
copyFileFromCommentables mode userId' userIdent' fromFile toFile name emptyPH = do
    cleanCommentPaths mode userId' toFile
    createDirectoryIfMissing False $ takeDirectory toFile
    commentHashFile <- BC.unpack <$> B.readFile fromFile
    commentFolder <- BC.unpack <$> B.readFile commentHashFile
    Just (project :: Project) <- decodeStrict <$>
      B.readFile (take (length commentFolder - 9) commentFolder)
    _ <- newCollaboratedProject mode userId' userIdent' name toFile $
      Project (projectSource project) emptyPH
    return ()

copyFolderFromCommentables :: BuildMode -> Text -> Text -> FilePath -> FilePath -> Text -> Value -> IO ()
copyFolderFromCommentables mode userId' userIdent' fromDir toDir name emptyPH = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \ f -> do
        let file = takeFileName f
        case isSuffixOf ".info" file of
            True -> return ()
            False -> do
                let toFile = toDir </> take 3 file </> file
                fileName <- B.readFile (f <.> "info")
                copyFileFromCommentables mode userId' userIdent' f toFile fileName emptyPH
    dirDirs <- dirFilter dirList 'D'
    forM_ dirDirs $ \d -> do
        dirName <- T.decodeUtf8 <$> B.readFile (d </> "dir.info")
        let newToDir = toDir </> (dirBase . nameToDirId $ dirName)
        copyFolderFromCommentables mode userId' userIdent' d newToDir dirName emptyPH

copyFileFromSelf :: BuildMode -> Text -> Text -> FilePath -> FilePath -> ByteString -> IO ()
copyFileFromSelf mode userId' userIdent' fromFile toFile name = do
    cleanCommentPaths mode userId' toFile
    createDirectoryIfMissing False $ takeDirectory toFile
    collabPath <- BC.unpack <$> B.readFile fromFile
    Just (project :: Project) <- decodeStrict <$> B.readFile collabPath
    _ <- newCollaboratedProject mode userId' userIdent' name toFile project
    return ()

copyFolderFromSelf :: BuildMode -> Text -> Text -> FilePath -> FilePath -> Text -> IO ()
copyFolderFromSelf mode userId' userIdent' fromDir toDir name = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \f -> do
        let file = takeFileName f
        fileBool <- doesFileExist f
        case (fileBool, isSuffixOf ".cw" file) of
            (True, True) -> do
                let toFile = toDir </> take 3 file </> file
                name' <- B.readFile $ f <.> "info"
                copyFileFromSelf mode userId' userIdent' f toFile name'
            (_, _) -> return ()
    dirDirs <- dirFilter dirList 'D'
    forM_ dirDirs $ \d -> do
        dirName <- T.decodeUtf8 <$> B.readFile (d </> "dir.info")
        let newToDir = toDir </> (dirBase . nameToDirId $ dirName)
        copyFolderFromSelf mode userId' userIdent' d newToDir dirName

moveFileFromCommentables :: Text -> FilePath -> FilePath -> Text -> IO ()
moveFileFromCommentables userId' fromFile toFile name = do
    removeUserFromComments userId' toFile
    createDirectoryIfMissing False $ takeDirectory toFile
    mapM_ (\x -> renameFile (fromFile <.> x) (toFile <.> x)) ["", "info"]
    cleanBaseDirectory fromFile
    correctUserPathInComments userId' toFile
    B.writeFile (toFile <.> "info") $ T.encodeUtf8 name

moveFolderFromCommentables :: BuildMode -> Text -> FilePath -> FilePath -> Text -> IO ()
moveFolderFromCommentables mode userId' fromDir toDir name = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \f -> do
        let file = takeFileName f
        case isSuffixOf ".info" file of
            True -> return ()
            False -> do
                let toFile = toDir </> take 3 file </> file
                fileName <- T.decodeUtf8 <$> B.readFile (f <.> "info")
                moveFileFromCommentables userId' f toFile fileName
    dirDirs <- dirFilter dirList 'D'
    forM_ dirDirs $ \ d -> do
        dirName <- T.decodeUtf8 <$> B.readFile (d </> "dir.info")
        let newToDir = toDir </> (dirBase . nameToDirId $ dirName)
        moveFolderFromCommentables mode userId' d newToDir dirName
    removeDirectoryIfExists fromDir
    cleanBaseDirectory fromDir

moveFileFromSelf :: BuildMode -> Text -> FilePath -> FilePath -> Text -> IO ()
moveFileFromSelf mode userId' fromFile toFile name = do
    cleanCommentPaths mode userId' toFile
    createDirectoryIfMissing False $ takeDirectory toFile
    mapM_ (\x -> renameFile (fromFile <.> x) (toFile <.> x)) ["", "info"]
    cleanBaseDirectory fromFile
    B.writeFile (toFile <.> "info") $ T.encodeUtf8 name
    modifyCollabPathIfReq mode userId' fromFile toFile

moveFolderFromSelf :: BuildMode -> Text -> FilePath -> FilePath -> Text -> IO ()
moveFolderFromSelf mode userId' fromDir toDir name = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \ f -> do
        let file = takeFileName f
        fileBool <- doesFileExist f
        case (fileBool, isSuffixOf ".cw" file) of
            (True, True) -> do
                let toFile = toDir </> take 3 file </> file
                name' <- T.decodeUtf8 <$> B.readFile (f <.> "info")
                moveFileFromSelf mode userId' f toFile name'
            (_, _) -> return ()
    dirDirs <- dirFilter dirList 'D'
    forM_ dirDirs $ \d -> do
        dirName <- T.decodeUtf8 <$> B.readFile (d </> "dir.info")
        let newToDir = toDir </> (dirBase . nameToDirId $ dirName)
        moveFolderFromSelf mode userId' d newToDir dirName
    removeDirectoryIfExists fromDir
    cleanBaseDirectory fromDir

correctUserPathInComments :: Text -> FilePath -> IO ()
correctUserPathInComments userId' userPath = do
    fileBool <- doesFileExist userPath
    case fileBool of
        True -> do
            commentHashFile <- BC.unpack <$> B.readFile userPath
            Just (currentUsers :: [UserDump]) <- decodeStrict <$>
              B.readFile (commentHashFile <.> "users")
            let newUsr usr = usr { upath = T.pack userPath }
                newUsers = map (\usr -> if uuserId usr /= userId' then usr
                                                                  else newUsr usr) currentUsers
            B.writeFile (commentHashFile <.> "users") $
              LB.toStrict . encode $ newUsers
        False -> return ()
