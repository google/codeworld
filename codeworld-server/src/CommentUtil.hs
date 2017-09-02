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

module CommentUtil where

import           Control.Monad
import           Control.Exception
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

import Collaboration
import DataUtil
import Model

newtype CommentId = CommentId { unCommentId :: Text } deriving Eq

commentHashRootDir :: BuildMode -> FilePath
commentHashRootDir (BuildMode m) = "data" </> m </> "commentHash"

commentRootDir :: BuildMode -> Text -> FilePath -> ProjectId -> FilePath
commentRootDir mode userId' path projectId =
    userProjectDir mode userId' </> path </> projectFile projectId <.> "comments"

sharedCommentsDir :: BuildMode -> Text -> FilePath
sharedCommentsDir mode userId' = userProjectDir mode userId' </> "commentables"

commentHashLink :: CommentId -> FilePath
commentHashLink (CommentId c) = let s = T.unpack c in take 3 s </> s

commentProjectLink :: ProjectId -> FilePath
commentProjectLink projectId = take (length file - 3) file
  where file = projectFile projectId

nameToCommentHash :: FilePath -> CommentId
nameToCommentHash = CommentId . hashToId "C" . BC.pack

ensureCommentHashDir :: BuildMode -> CommentId -> IO ()
ensureCommentHashDir mode (CommentId c) = createDirectoryIfMissing True dir
  where dir = commentHashRootDir mode </> take 3 (T.unpack c)

ensureSharedCommentsDir :: BuildMode -> Text -> IO ()
ensureSharedCommentsDir mode userId' = createDirectoryIfMissing True dir
  where dir = sharedCommentsDir mode userId'

cleanCommentPaths :: BuildMode -> Text -> FilePath -> IO ()
cleanCommentPaths mode userId' file = do
    removeProjectIfExists mode userId' file

cleanCommentHashPath :: Text -> FilePath -> IO ()
cleanCommentHashPath userId' commentFolder = do
    dirBool <- doesDirectoryExist commentFolder
    case dirBool of
        True -> do
            let commentHash = nameToCommentHash commentFolder
                commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
            removeFileIfExists commentHashPath
            Just (currentUsers :: [UserDump]) <- decodeStrict <$>
              B.readFile (commentHashPath <.> "users")
            forM_ currentUsers $ \u -> do
                case uuserId == userId' of
                    True -> return ()
                    False -> do
                        removeFileIfExists . T.unpack $ upath u
                        removeFileIfExists . T.unpack $ upath u <.> "info"
                        cleanBaseDirectory . T.unpack $ upath u
            removeFileIfExists $ commentHashPath <.> "users"
            cleanBaseDirectory commentHashPath
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

correctOwnerPathInComments :: BuildMode -> Text -> FilePath -> FilePath -> IO ()
correctOwnerPathInComments mode userId' userPath commentFolder = do
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- decodeStrict <$>
      B.readFile (commentHashPath <.> "users")
    let newUsr usr = usr { upath = T.pack userPath }
        newUsers = map (\usr -> if uuserId usr /= userId' then usr
                                                          else newUsr usr) currentUsers
    B.writeFile (commentHashPath <.> "users") $
      LB.toStrict . encode $ newUsers

removeOwnerPathInComments :: BuildMode -> Text -> FilePath -> IO ()
removeOwnerPathInComments mode userId' commentFolder = do
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- decodeStrict <$>
      B.readFile (commentHashPath <.> "users")
    B.writeFile (commentHashPath <.> "users") $
      LB.toStrict . encode $ filter (\x -> uuserId x /= userId') currentUsers

updateSharedCommentPath :: FilePath -> IO ()
updateSharedCommentPath oldCommentFolder commentFolder = do
    let oldCommentHash = nameToCommentHash oldCommentFolder
        oldCommentHashPath = commentHashRootDir mode </> commentHashLink oldCommentHash
        commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    moveDirIfExists (takeDirectory oldCommentHashPath) $ takeDirectory commentHashPath
    B.writeFile commentHashPath commentFolder
    Just (currentUsers :: [UserDump]) <- decodeStrict <$>
      B.readFile (commentHashPath <.> "users")
    forM_ currentUsers $ \u -> do
        case utype u of
            "owner" -> return ()
            _ -> B.writeFile (T.unpack . upath $ u) $ BC.pack commentHashPath

copyFileFromCommentables :: BuildMode -> Text -> Text -> FilePath -> FilePath -> ByteString -> Value -> IO ()
copyFileFromCommentables mode userId' userIdent' fromFile toFile name emptyPH = do
    cleanCommentPaths mode userId' toFile
    createDirectoryIfMissing False $ takeDirectory toFile
    commentHashFile <- BC.unpack <$> B.readFile fromFile
    commentFolder <- BC.unpack <$> B.readFile commentHashFile
    Just (project :: Project) <- decodeStrict <$>
      B.readFile (take (length commentFolder - 9) commentFolder)
    newCollaboratedProject mode userId' userIdent' name toFile $
      Project (projectSource project) emptyPH

copyFolderFromCommentables :: BuildMode -> Text -> Text -> FilePath -> FilePath -> Text -> Value -> IO ()
copyFolderFromCommentables mode userId' userIdent' fromDir toDir name emptyPH = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \ f -> do
        let file = takeFileName f
        case isSuffixOf ".info" of
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
    newCollaboratedProject mode userId' userIdent' name toFile project

copyFolderFromSelf :: BuildMode -> Text -> FilePath -> FilePath -> Text -> IO ()
copyFolderFromSelf mode userId' fromDir toDir name = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \f -> do
        let file = takeFileName f
        fileBool <- doesFileExist f
        case (fileBool, isSuffixOf ".cw") of
            (True, True) -> do
                let toFile = toDir </> take 3 file </> file
                name <- B.readFile $ f <.> "info"
                copyFileFromSelf mode userId' userIdent' f toFile name
            (_, _) -> return ()
    dirDirs <- dirFilter dirList 'D'
    forM_ dirDirs $ \d -> do
        dirName <- T.decodeUtf8 <$> B.readFile (d </> "dir.info")
        let newToDir = toDir </> (dirBase . nameToDirId $ dirName)
        copyFolderFromSelf mode userId' userIdent' d newToDir dirName

moveFileFromCommentables :: Text -> Text -> FilePath -> FilePath -> Text -> IO ()
moveFileFromCommentables userId' userIdent' fromFile toFile name = do
    removeUserFromComments userId' toFile
    createDirectoryIfMissing False $ takeDirectory toFile
    mapM_ (\x -> renameFile (fromFile <.> x) (toFile <.> x)) ["", "info"]
    cleanBaseDirectory fromFile
    correctUserPathInComments userId' toFile
    B.writeFile (toFile <.> "info") $ T.encodeUtf8 name

moveFolderFromCommentables :: BuildMode -> Text -> Text -> FilePath -> FilePath -> Text -> IO ()
moveFolderFromCommentables mode userId' userIdent' fromDir toDir name = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \f -> do
        let file = takeFileName f
        case isSuffixOf ".info" of
            True -> return ()
            False -> do
                let toFile = toDir </> take 3 file </> file
                fileName <- T.decodeUtf8 <$> B.readFile (f <.> "info")
                moveFileFromCommentables userId' userIdent' f toFile fileName
    dirDirs <- dirFilter dirList 'D'
    forM_ dirDirs $ \ d -> do
        dirName <- T.decodeUtf8 <$> B.readFile (d </> "dir.info")
        let newToDir = toDir </> (dirBase . nameToDirId $ dirName)
        moveFolderFromCommentables mode userId' userIdent' d newToDir dirName
    removeDirectoryIfExists fromDir
    cleanBaseDirectory fromDir

moveFileFromSelf :: BuildMode -> Text -> Text -> FilePath -> FilePath -> Text -> IO ()
moveFileFromSelf mode userId' userIdent' fromFile toFile name = do
    cleanCommentPaths mode userId' toFile
    createDirectoryIfMissing False $ takeDirectory toFile
    mapM_ (\x -> renameFile (fromFile <.> x) (toFile <.> x)) ["", "info"]
    cleanBaseDirectory fromFile
    B.writeFile (toFile <.> "info") $ T.decodeUtf8 name
    modifyCollabPathIfReq mode userId' fromFile toFile

moveFolderFromSelf :: BuildMode -> Text -> FilePath -> FilePath -> Text -> IO ()
moveFolderFromSelf mode userId' fromDir toDir name = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \ f -> do
        let file = takeFileName f
        fileBool <- doesFileExist f
        case (fileBool, isSuffixOf ".cw") of
            (True, True) -> do
                let toFile = toDir </> take 3 file </> file
                name <- T.decodeUtf8 <$> B.readFile $ f <.> "info"
                moveFileFromSelf mode userId' userIdent' f toFile name
            (_, _) -> return ()
    dirDirs <- dirFilter dirList 'D'
    forM_ dirDirs $ \d -> do
        dirName <- T.decodeUtf8 <$> B.readFile (d </> "dir.info")
        let newToDir = toDir </> (dirBase . nameToDirId $ dirName)
        moveFolderFromSelf mode userId' userIdent' d newToDir dirName
    removeDirectoryIfExists fromDir
    cleanBaseDirectory fromDir

createNewVersionIfReq :: Text -> Int -> FilePath -> IO (Either String ())
createNewVersionIfReq latestSource versionNo' commentFolder = do
    currentVersions :: [Int] <- reverse . sort . map read <$>
      listDirectory (commentFolder <.> "versions")
    let currentVersion = currentVersions !! 0
    currentSource <- T.decodeUtf8 <$>
      B.readFile (commentFolder <.> "versions" </> show currentVersion)
    case (currentSource == latestSource, currentVersion > versionNo') of
        (_, True) -> return $ Left "Cannot Edit A Previous Version."
        (True, _) -> return $ Right ()
        (False, _) -> do
            currentLines :: [Int] <- delete 0 . fmap read <$> listDirectory commentFolder
            commentVersionLists :: [[[CommentDesc]]] <- mapM (\x -> versions . fromJust . decodeStrict <$>
              B.readFile (commentFolder </> show x)) currentLines
            let hasComments = foldr (\l acc ->
                                if (l !! currentVersion /= [])  then True else acc
                                ) False commentVersionLists
            case hasComments of
                True -> do
                    B.writeFile (commentFolder <.> "versions" </> show (currentVersion + 1)) $
                    T.encodeUtf8 latestSource
                    ensureVersionLines (currentVersion + 1) commentFolder
                    return $ Right ()
                False -> do
                    B.writeFile (commentFolder <.> "versions" </> show currentVersion) $
                    T.encodeUtf8 latestSource
                    ensureVersionLines currentVersion commentFolder
                    return $ Right ()

updateUserVersionLS :: Text -> FilePath -> IO ()
updateUserVersionLS userIdent' commentFolder = do
    currentLines :: [Int] <- delete 0 . fmap read <$> listDirectory commentFolder
    currentVersions :: [Int] <- fmap read <$> (listDirectory $ commentFolder <.> "versions")
    commentVersionLists :: [[[CommentDesc]]] <- mapM (\x -> versions . fromJust . decodeStrict <$>
      B.readFile (commentFolder </> show x)) currentLines
    let versionLS = map (\v -> VersionLS v . LineStatuses $ foldr (\l acc ->
                      LineStatus (currentLines !! (fromJust $
                        l `elemIndex` commentVersionLists))
                        (if (l !! v /= []) then "unread" else "read") : acc
                      ) [] commentVersionLists
                    ) currentVersions
    B.writeFile (commentFolder <.> "users" </> T.unpack userIdent') $
      LB.toStrict . encode $ VersionLS_ versionLS

ensureVersionLines :: Int -> FilePath -> IO ()
ensureVersionLines versionNo' commentFolder = do
    totalLines <- (length . lines . BC.unpack) <$>
      (B.readFile $ commentFolder <.> "versions" </> show versionNo')
    currentLines :: [Int] <- delete 0 . fmap read <$> listDirectory commentFolder
    mapM_ (\x -> do
        fileBool <- doesFileExist $ commentFolder </> show x
        case fileBool of
            True -> do
                Just (currentLC :: LineComment) <- decodeStrict <$>
                  B.readFile (commentFolder </> show x)
                let currLength = length . versions $ currentLC
                let newLC = LineComment x (versions currentLC ++
                              replicate (versionNo' - currLength + 1) [])
                B.writeFile (commentFolder </> show x) $ LB.toStrict . encode $ newLC
            False -> do
                let newLC = LineComment x (replicate (versionNo' + 1) [])
                B.writeFile (commentFolder </> show x) $ LB.toStrict . encode $ newLC
    )[1..totalLines `max` length currentLines]
    currentUsers <- map T.pack <$> listDirectory (commentFolder <.> "users")
    forM_ currentUsers $ \u -> do
        Just (versionLS :: [VersionLS]) <- fmap getVersionLS <$> decodeStrict <$>
          B.readFile (commentFolder <.> "users" </> T.unpack u)
        let newVersionLS = versionLS ++ if (length versionLS == versionNo' + 1) then []
                                        else [VersionLS versionNo' (LineStatuses [])]
        B.writeFile (commentFolder <.> "users" </> T.unpack u) $
          LB.toStrict . encode . VersionLS_ $ newVersionLS

addNewUser :: Text -> Text -> FilePath -> FilePath -> FilePath -> IO (Either String ())
addNewUser userId' userIdent' name userPath commentHashPath = do
    let identAllowed = foldl (\acc l ->
                         if l `elem` (T.unpack userIdent') then False else acc
                       ) True ['/', '.', '+']
    -- make user id unique instead of user identifier only
    case identAllowed of
      False -> return $ Left "User Identifier Has Unallowed Char(/+.)"
      True -> do
        fileBool <- doesFileExist commentHashPath
        case fileBool of
          True -> do
            Just (currentUsers :: [UserDump]) <- decodeStrict <$>
              B.readFile (commentHashPath <.> "users")
            let currentIdents = map uuserIdent currentUsers
            case userIdent' `elem` currentIdents of
              False -> do
                createDirectoryIfMissing False $ takeDirectory userPath
                B.writeFile userPath $ BC.pack commentHashPath
                B.writeFile (userPath <.> "info") $ BC.pack name
                B.writeFile (commentHashPath <.> "users") $ LB.toStrict . encode $ UserDump
                  userId' userIdent' (T.pack userPath) : currentUsers
                commentFolder <- BC.unpack <$> B.readFile commentHashPath
                updateUserVersionLS userIdent' commentFolder
                return $ Right ()
              True -> return $ Left "User Identifier Already Exists"
          False -> return $ Left "File Does Not Exists"

addNewOwner :: UserDump -> FilePath -> IO (Either String ())
addNewOwner userDump commentFolder = do
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- decodeStrict <$>
      B.readFile (commentHashPath <.> "users")
    case (uuserIdent userDump) `elem` currentUsers of
        True -> return $ Left "User Identifier Already Exists"
        False -> do
            B.writeFile (commentHashPath <.> "users") $
              LB.toStrict . encode $ userDump : currentUsers
            updateUserVersionLS (uuserIdent userDump) commentFolder
            return $ Right ()

addCommentFunc :: BuildMode -> UserDump -> Project -> FilePath -> IO ()
addCommentFunc mode userDump project commentFolder = do
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    createDirectoryIfMissing False commentFolder
    ensureCommentHashDir mode commentHash
    B.writeFile commentHashPath $ BC.pack commentFolder
    B.writeFile (commentHashPath <.> "users") $
      LB.toStrict . encode $ userDump : []
    createDirectoryIfMissing False $ commentFolder <.> "users"
    createDirectoryIfMissing False $ commentFolder <.> "versions"
    B.writeFile (commentFolder <.> "versions" </> "0") $ T.encodeUtf8 . projectSource $ project
    ensureVersionLines 0 commentFolder
    updateUserVerisonLS (uuserIdent userDump) commentFolder

listUnreadComments :: Text -> FilePath -> Int -> IO [Int]
listUnreadComments userIdent' commentFolder versionNo' = do
    Just (versionLS :: VersionLS_) <- decodeStrict <$>
      B.readFile (commentFolder <.> "users" </> T.unpack userIdent')
    let currentLineList = listStatuses . versionStatus $ (getVersionLS versionLS) !! versionNo'
        unreadLineList = foldr (\l acc ->
                           if ((T.unpack . lstatus $ l) == "unread") then (llineNo l) : acc
                                                                     else acc)
                           [] currentLineList
    return unreadLineList

getLineComment :: FilePath -> Int -> Int -> IO [CommentDesc]
getLineComment commentFolder lineNo' versionNo' = do
    Just (lc :: LineComment) <- decodeStrict <$> B.readFile (commentFolder </> show lineNo')
    return $ (versions lc) !! versionNo'

markReadComments :: Text -> FilePath -> Int -> Int -> IO ()
markReadComments userIdent' commentFolder lineNo' versionNo' = do
    Just (versionLS :: VersionLS_) <- decodeStrict <$>
      B.readFile (commentFolder <.> "users" </> T.unpack userIdent')
    let currentLineList = listStatuses . versionStatus $ (getVersionLS versionLS) !! versionNo'
        newLineList = VersionLS versionNo' . LineStatuses . map (\x ->
                        if llineNo x == lineNo' then LineStatus lineNo' "read"
                                                else x) $ currentLineList
        spnll = splitAt versionNo' (getVersionLS versionLS)
    B.writeFile (commentFolder <.> "users" </> T.unpack userIdent') $
      LB.toStrict . encode . VersionLS_ $ fst spnll ++ (newLineList : (tail $ snd spnll))

addCommentToFile :: FilePath -> Int -> Int -> CommentDesc -> IO ()
addCommentToFile commentFolder lineNo' versionNo' comment' = do
    Just (lc :: LineComment) <- decodeStrict <$> B.readFile (commentFolder </> show lineNo')
    let newComments = ((versions lc) !! versionNo') ++ [comment']
        spvn = splitAt versionNo' (versions lc)
    B.writeFile (commentFolder </> show lineNo') $ LB.toStrict . encode $ LineComment lineNo' $
      fst spvn ++ (newComments : (tail $ snd spvn))

markUnreadComments :: Text -> FilePath -> Int -> Int -> IO ()
markUnreadComments userIdent' commentFolder lineNo' versionNo' = do
    currentUsers <- delete (T.unpack userIdent') <$> listDirectory (commentFolder <.> "users")
    forM_ currentUsers $ \u -> do
        Just (versionLS :: VersionLS_) <- decodeStrict <$>
          B.readFile (commentFolder <.> "users" </> u)
        let currentLineList = listStatuses . versionStatus $
                                (getVersionLS versionLS) !! versionNo'
            newLineList = VersionLS versionNo' . LineStatuses . map (\x ->
                            if llineNo x == lineNo' then LineStatus lineNo' "unread"
                                                    else x) $ currentLineList
            spnll = splitAt versionNo' (getVersionLS versionLS)
        B.writeFile (commentFolder <.> "users" </> T.unpack userIdent') $
          LB.toStrict . encode . VersionLS_ $ fst spnll ++ (newLineList : (tail $ snd spnll))

addReplyToComment :: FilePath -> Int -> Int -> CommentDesc -> ReplyDesc -> IO ()
addReplyToComment commentFolder lineNo' versionNo' cd rd = do
    Just (lc :: LineComment) <- decodeStrict <$> B.readFile (commentFolder </> show lineNo')
    let Just ind = elemIndex cd ((versions lc) !! versionNo')
        newcd = CommentDesc (cuserIdent cd) (cdateTime cd) (cstatus cd)
                  (comment cd) (replies cd ++ [rd])
        splc = splitAt versionNo' $ versions lc
        spvn = splitAt ind ((versions lc) !! versionNo')
        newvn = fst spvn ++ (newcd : (tail $ snd spvn))
        newlc = LineComment lineNo' $ fst splc ++ (newvn : (tail $ snd splc))
    B.writeFile (commentFolder </> show lineNo') $ LB.toStrict . encode $ newlc

deleteCommentFromFile :: FilePath -> Int -> Int -> CommentDesc -> IO ()
deleteCommentFromFile commentFolder lineNo' versionNo' cd = do
    Just (lc :: LineComment) <- decodeStrict <$> B.readFile (commentFolder </> show lineNo')
    let Just ind = elemIndex cd ((versions lc) !! versionNo')
        newcd = CommentDesc "none" (cdateTime cd) "deleted" "none" (replies cd)
        splc = splitAt versionNo' $ versions lc
        spvn = splitAt ind ((versions lc) !! versionNo')
        newvn = fst spvn ++ (if (length $ replies cd) /= 0
                            then newcd : (tail $ snd spvn)
                            else tail $ snd spvn)
        newlc = LineComment lineNo' $ fst splc ++ (newvn : (tail $ snd splc))
    B.writeFile (commentFolder </> show lineNo') $ LB.toStrict . encode $ newlc

deleteReplyFromComment :: FilePath -> Int -> Int -> CommentDesc -> ReplyDesc -> IO ()
deleteReplyFromComment commentFolder lineNo' versionNo' cd rd = do
    Just (lc :: LineComment) <- decodeStrict <$> B.readFile (commentFolder </> show lineNo')
    let Just cdInd = elemIndex cd ((versions lc) !! versionNo')
        Just rdInd = elemIndex rd (replies cd)
        spvn = splitAt cdInd $ (versions lc) !! versionNo'
        splc = splitAt versionNo' $ versions lc
        spcd = splitAt rdInd $ replies cd
        newcd = CommentDesc (cuserIdent cd) (cdateTime cd) (cstatus cd) (comment cd) $
          (fst spcd) ++ (tail $ snd spcd)
        newvn = fst spvn ++ (if (length $ replies newcd) /= 0
                            then newcd : (tail $ snd spvn)
                            else if cstatus newcd == "deleted"
                                 then (tail $ snd spvn)
                                 else newcd : (tail $ snd spvn))
        newlc = LineComment lineNo' $ fst splc ++ (newvn : (tail $ snd splc))
    B.writeFile (commentFolder </> show lineNo') $ LB.toStrict . encode $ newlc
