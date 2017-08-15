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
import           Data.Aeson
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

removeCommentHash :: BuildMode -> FilePath -> IO ()
removeCommentHash mode commentFolder = do
    let commentHash = nameToCommentHash commentFolder
    removeFileIfExists $ commentHashRootDir mode </> commentHashLink commentHash

cleanCommentPaths :: BuildMode -> FilePath -> IO ()
cleanCommentPaths mode commentFolder = do
    dirBool <- doesDirectoryExist commentFolder
    case dirBool of
      True -> do
        mapM_ (\x -> removeDirectoryIfExists $ commentFolder <.> x) ["", "users", "versions"]
        removeFileIfExists $ take (length commentFolder - 9) commentFolder
        cleanBaseDirectory commentFolder
        let commentHash = nameToCommentHash commentFolder
            commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
        removeFileIfExists commentHashPath
        Just (currentUsers :: [UserDump]) <- decode <$>
          LB.readFile (commentHashPath <.> "users")
        forM_ currentUsers $ \u -> do
            removeFileIfExists $ T.unpack (upath u)
            removeFileIfExists $ T.unpack (upath u) <.> "info"
            cleanBaseDirectory $ T.unpack (upath u)
        removeFileIfExists $ commentHashPath <.> "users"
        cleanBaseDirectory commentHashPath
      False -> return ()

deleteFolderWithComments :: BuildMode -> Text -> FilePath -> IO (Either String ())
deleteFolderWithComments mode userId' finalDir = do
    dirBool <- doesDirectoryExist finalDir
    case dirBool of
      True -> do
        case finalDir == "commentables" of
          True -> return $ Left "`commentables` Directory Cannot Be Deleted"
          False -> do
            let dir' = userProjectDir mode userId' </> finalDir
            allFilePaths <- getFilesRecursive dir'
            case length (splitDirectories finalDir) of
              x | x == 0 -> return $ Left "Root Directory Cannot Be Deleted"
                | (x /= 0) && ((splitDirectories finalDir) !! 0 == "commentables") -> do
                   mapM_ (removeUserFromComments userId') allFilePaths
                   removeDirectoryIfExists dir'
                   cleanBaseDirectory dir'
                   return $ Right ()
                | otherwise -> do
                   mapM_ (\t -> cleanCommentPaths mode $ t <.> "comments") allFilePaths
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
        Just (currentUsers :: [UserDump]) <- decode <$>
          LB.readFile (commentHashFile <.> "users")
        let currentUserIds = map uuserId currentUsers
            currentUser = currentUsers !! (fromJust $ userId' `elemIndex` currentUserIds)
        removeFileIfExists $ commentFolder <.> "users" </> T.unpack (uuserIdent currentUser)
        LB.writeFile (commentHashFile <.> "users") $
          encode (delete currentUser currentUsers)
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
        Just (currentUsers :: [UserDump]) <- decode <$>
          LB.readFile (commentHashFile <.> "users")
        let newUsr usr = UserDump userId' (uuserIdent usr) $ T.pack userPath
            newUsers = map (\usr -> if uuserId usr /= userId' then usr
                                                              else newUsr usr) currentUsers
        LB.writeFile (commentHashFile <.> "users") $
          encode newUsers
      False -> return ()

copyFileFromCommentables :: BuildMode -> Text -> FilePath -> FilePath -> Text -> Value -> IO ()
copyFileFromCommentables mode userId' fromFile toFile name emptyPH = do
    cleanCommentPaths mode $ toFile <.> "comments"
    createDirectoryIfMissing False $ takeDirectory toFile
    commentHashFile <- BC.unpack <$> B.readFile fromFile
    commentFolder <- BC.unpack <$> B.readFile commentHashFile
    Just (project :: Project) <- decode <$>
      LB.readFile (take (length commentFolder - 9) commentFolder)
    LB.writeFile toFile $ encode
      (Project name (projectSource project) emptyPH)
    addSelf mode userId' "Anonymous Owner" $ toFile <.> "comments"

copyFolderFromCommentables :: BuildMode -> Text -> FilePath -> FilePath -> Text -> Value -> IO ()
copyFolderFromCommentables mode userId' fromDir toDir name emptyPH = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \ f -> do
        let file = takeFileName f
        case isSuffixOf ".info" (drop 23 file) of
          True -> return ()
          False -> do
            let toFile = toDir </> take 3 file </> file
            fileName <- T.decodeUtf8 <$> B.readFile (f <.> "info")
            copyFileFromCommentables mode userId' f toFile fileName emptyPH
    dirDirs <- dirFilter dirList 'D'
    forM_ dirDirs $ \ d -> do
        dirName <- T.decodeUtf8 <$> B.readFile (d </> "dir.info")
        let newToDir = toDir </> (dirBase . nameToDirId $ dirName)
        copyFolderFromCommentables mode userId' d newToDir dirName emptyPH

copyFileFromSelf :: BuildMode -> Text -> FilePath -> FilePath -> Text -> IO ()
copyFileFromSelf mode userId' fromFile toFile name = do
    cleanCommentPaths mode $ toFile <.> "comments"
    createDirectoryIfMissing False $ takeDirectory toFile
    Just (project :: Project) <- decode <$> LB.readFile fromFile
    LB.writeFile toFile $ encode
      (Project name (projectSource project) (projectHistory project))
    addSelf mode userId' "Anonymous Owner" $ toFile <.> "comments"

copyFolderFromSelf :: BuildMode -> Text -> FilePath -> FilePath -> Text -> IO ()
copyFolderFromSelf mode userId' fromDir toDir name = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \f -> do
        let file = takeFileName f
        fileBool <- doesFileExist f
        case (fileBool, isSuffixOf ".cw" (drop 23 file)) of
          (True, True) -> do
            let toFile = toDir </> take 3 file </> file
            Just (project :: Project) <- decode <$> LB.readFile f
            copyFileFromSelf mode userId' f toFile $ projectName project
          (_, _) -> return ()
    dirDirs <- dirFilter dirList 'D'
    forM_ dirDirs $ \ d -> do
        dirName <- T.decodeUtf8 <$> B.readFile (d </> "dir.info")
        let newToDir = toDir </> (dirBase . nameToDirId $ dirName)
        copyFolderFromSelf mode userId' d newToDir dirName

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
    forM_ dirFiles $ \ f -> do
        let file = takeFileName f
        case isSuffixOf ".info" (drop 23 file) of
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
    cleanCommentPaths mode $ toFile <.> "comments"
    createDirectoryIfMissing False $ takeDirectory toFile
    mapM_ (\x -> renameDirectory (fromFile <.> x) (toFile <.> x)) $
      map ("comments" <.>) ["", "users", "versions"]
    renameFile fromFile toFile
    cleanBaseDirectory fromFile
    Just (project :: Project) <- decode <$> LB.readFile toFile
    LB.writeFile toFile $ encode
      (Project name (projectSource project) (projectHistory project))
    let fromCommentHash = nameToCommentHash $ fromFile <.> "comments"
        toCommentHash = nameToCommentHash $ toFile <.> "comments"
        fromCommentHashPath = commentHashRootDir mode </> commentHashLink fromCommentHash
        toCommentHashPath = commentHashRootDir mode </> commentHashLink toCommentHash
    ensureCommentHashDir mode toCommentHash
    renameFile fromCommentHashPath toCommentHashPath
    renameFile (fromCommentHashPath <.> "users") (toCommentHashPath <.> "users")
    cleanBaseDirectory fromCommentHashPath
    -- change user paths to toCommentHashPath
    Just (currentUsers :: [UserDump]) <- decode <$>
      LB.readFile (toCommentHashPath <.> "users")
    mapM_ (\u ->
      if (uuserId u /= userId') then B.writeFile (T.unpack $ upath u) $ BC.pack toCommentHashPath
                                else return ()
      ) currentUsers

moveFolderFromSelf :: BuildMode -> Text -> FilePath -> FilePath -> Text -> IO ()
moveFolderFromSelf mode userId' fromDir toDir name = do
    createNewFolder mode userId' toDir $ T.unpack name
    dirList <- listDirectoryWithPrefix fromDir
    dirFiles <- dirFilter dirList 'S'
    forM_ dirFiles $ \ f -> do
        let file = takeFileName f
        fileBool <- doesFileExist f
        case (fileBool, isSuffixOf ".cw" (drop 23 file)) of
          (True, True) -> do
            let toFile = toDir </> take 3 file </> file
            Just (project :: Project) <- decode <$> LB.readFile f
            moveFileFromSelf mode userId' f toFile $ projectName project
          (_, _) -> return ()
    dirDirs <- dirFilter dirList 'D'
    forM_ dirDirs $ \ d -> do
        dirName <- T.decodeUtf8 <$> B.readFile (d </> "dir.info")
        let newToDir = toDir </> (dirBase . nameToDirId $ dirName)
        moveFolderFromSelf mode userId' d newToDir dirName
    removeDirectoryIfExists fromDir
    cleanBaseDirectory fromDir

createNewVersionIfReq :: Text -> FilePath -> IO ()
createNewVersionIfReq latestSource commentFolder = do
    currentVersions :: [Int] <- reverse . sort . map read <$>
      listDirectory (commentFolder <.> "versions")
    let currentVersion = currentVersions !! 0
    currentSource <- T.decodeUtf8 <$>
      B.readFile (commentFolder <.> "versions" </> show currentVersion)
    case currentSource == latestSource of
      True -> return ()
      False -> do
        currentLines :: [Int] <- delete 0 . fmap read <$> listDirectory commentFolder
        commentVersionLists :: [[[CommentDesc]]] <- mapM (\x -> versions . fromJust . decode <$>
          LB.readFile (commentFolder </> show x)) currentLines
        let hasComments = foldr (\l acc ->
                            if (l !! currentVersion /= [])  then True else acc
                            ) False commentVersionLists
        case hasComments of
          True -> do
            B.writeFile (commentFolder <.> "versions" </> show (currentVersion + 1)) $
              T.encodeUtf8 latestSource
            ensureVersionLines (currentVersion + 1) commentFolder
          False -> return ()

updateUserVersionLS :: Text -> FilePath -> IO ()
updateUserVersionLS userIdent' commentFolder = do
    currentLines :: [Int] <- delete 0 . fmap read <$> listDirectory commentFolder
    currentVersions :: [Int] <- fmap read <$> (listDirectory $ commentFolder <.> "versions")
    commentVersionLists :: [[[CommentDesc]]] <- mapM (\x -> versions . fromJust . decode <$>
      LB.readFile (commentFolder </> show x)) currentLines
    let versionLS = map (\v -> VersionLS v . LineStatuses $ foldr (\l acc ->
                      LineStatus (currentLines !! (fromJust $
                        l `elemIndex` commentVersionLists))
                        (if (l !! v /= []) then "unread" else "read") : acc
                      ) [] commentVersionLists
                    ) currentVersions
    LB.writeFile (commentFolder <.> "users" </> T.unpack userIdent') $
      encode $ VersionLS_ versionLS

ensureVersionLines :: Int -> FilePath -> IO ()
ensureVersionLines versionNo' commentFolder = do
    totalLines <- (length . lines . BC.unpack) <$>
      (B.readFile $ commentFolder <.> "versions" </> show versionNo')
    currentLines :: [Int] <- delete 0 . fmap read <$> listDirectory commentFolder
    mapM_ (\x -> do
      fileBool <- doesFileExist $ commentFolder </> show x
      newLC <- case fileBool of
        True -> do
          Just (currentLC :: LineComment) <- decode <$>
            LB.readFile (commentFolder </> show x)
          return $ LineComment x (versions currentLC ++ [[]])
        False -> return $ LineComment x [[]]
      LB.writeFile (commentFolder </> show x) $ encode newLC)
        [1..totalLines `max` length currentLines]
    currentUsers <- map T.pack <$> listDirectory (commentFolder <.> "users")
    forM_ currentUsers (\u -> updateUserVersionLS u commentFolder)


addNewUser :: Text -> Text -> FilePath -> FilePath -> FilePath -> IO (Either String ())
addNewUser userId' userIdent' name userPath commentHashPath = do
    let identAllowed = foldl (\acc l ->
                         if l `elem` (T.unpack userIdent') then False else acc
                       ) True ['/', '.', '+']
    case identAllowed of
      True -> return $ Left "User Identifier Has Unallowed Char(/+.)"
      False -> do
        fileBool <- doesFileExist commentHashPath
        case fileBool of
          True -> do
            Just (currentUsers :: [UserDump]) <- decode <$>
              LB.readFile (commentHashPath <.> "users")
            let currentIdents = map uuserIdent currentUsers
            case userIdent' `elem` currentIdents of
              False -> do
                createDirectoryIfMissing False $ takeDirectory userPath
                B.writeFile userPath $ BC.pack commentHashPath
                B.writeFile (userPath <.> "info") $ BC.pack name
                LB.writeFile (commentHashPath <.> "users") $ encode (UserDump
                  userId' userIdent' (T.pack userPath) : currentUsers)
                commentFolder <- BC.unpack <$> B.readFile commentHashPath
                updateUserVersionLS userIdent' commentFolder
                return $ Right ()
              True -> return $ Left "User Identifier Already Exists"
          False -> return $ Left "File Does Not Exists"

addSelf :: BuildMode -> Text -> Text -> FilePath -> IO ()
addSelf mode userId' userIdent' commentFolder = do
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    createDirectoryIfMissing False commentFolder
    ensureCommentHashDir mode commentHash
    B.writeFile commentHashPath $ BC.pack commentFolder
    LB.writeFile (commentHashPath <.> "users") $ encode . UserDump
      userId' userIdent' $ T.pack $ take (length commentFolder - 9) commentFolder
    createDirectoryIfMissing False $ commentFolder <.> "users"
    createDirectoryIfMissing False $ commentFolder <.> "versions"
    Just (project :: Project) <- decode <$>
      (LB.readFile $ take (length commentFolder - 9) commentFolder)
    B.writeFile (commentFolder <.> "versions" </> "0") $ T.encodeUtf8 . projectSource $ project
    ensureVersionLines 0 commentFolder
    updateUserVersionLS userIdent' $ commentFolder

listUnreadComments :: Text -> FilePath -> Int -> IO [Int]
listUnreadComments userIdent' commentFolder versionNo' = do
    Just (versionLS :: VersionLS_) <- decode <$>
      LB.readFile (commentFolder <.> "users" </> T.unpack userIdent')
    let currentLineList = listStatuses . versionStatus $ (getVersionLS versionLS) !! versionNo'
        unreadLineList = foldr (\l acc ->
                           if ((T.unpack . lstatus $ l) == "unread") then (llineNo l) : acc
                                                                     else acc)
                           [] currentLineList
    return unreadLineList

getLineComment :: FilePath -> Int -> Int -> IO [CommentDesc]
getLineComment commentFolder lineNo' versionNo' = do
    Just (lc :: LineComment) <- decode <$> LB.readFile (commentFolder </> show lineNo')
    return $ (versions lc) !! versionNo'

markReadComments :: Text -> FilePath -> Int -> Int -> IO ()
markReadComments userIdent' commentFolder lineNo' versionNo' = do
    Just (versionLS :: VersionLS_) <- decode <$>
      LB.readFile (commentFolder <.> "users" </> T.unpack userIdent')
    let currentLineList = listStatuses . versionStatus $ (getVersionLS versionLS) !! versionNo'
        newLineList = VersionLS versionNo' . LineStatuses . map (\x ->
                        if llineNo x == lineNo' then LineStatus lineNo' "read"
                                                else x) $ currentLineList
        spnll = splitAt versionNo' (getVersionLS versionLS)
    LB.writeFile (commentFolder <.> "users" </> T.unpack userIdent') $
      encode . VersionLS_ $ fst spnll ++ (newLineList : (tail $ snd spnll))

addCommentToFile :: FilePath -> Int -> Int -> CommentDesc -> IO ()
addCommentToFile commentFolder lineNo' versionNo' comment' = do
    Just (lc :: LineComment) <- decode <$> LB.readFile (commentFolder </> show lineNo')
    let newComments = ((versions lc) !! versionNo') ++ [comment']
        spvn = splitAt versionNo' (versions lc)
    LB.writeFile (commentFolder </> show lineNo') $ encode (LineComment lineNo' $
      fst spvn ++ (newComments : (tail $ snd spvn)))

markUnreadComments :: Text -> FilePath -> Int -> Int -> IO ()
markUnreadComments userIdent' commentFolder lineNo' versionNo' = do
    currentUsers <- delete (T.unpack userIdent') <$> listDirectory (commentFolder <.> "users")
    forM_ currentUsers $ \u -> do
        Just (versionLS :: VersionLS_) <- decode <$>
          LB.readFile (commentFolder <.> "users" </> u)
        let currentLineList = listStatuses . versionStatus $
                                (getVersionLS versionLS) !! versionNo'
            newLineList = VersionLS versionNo' . LineStatuses . map (\x ->
                            if llineNo x == lineNo' then LineStatus lineNo' "unread"
                                                    else x) $ currentLineList
            spnll = splitAt versionNo' (getVersionLS versionLS)
        LB.writeFile (commentFolder <.> "users" </> T.unpack userIdent') $
          encode . VersionLS_ $ fst spnll ++ (newLineList : (tail $ snd spnll))

addReplyToComment :: FilePath -> Int -> Int -> CommentDesc -> ReplyDesc -> IO ()
addReplyToComment commentFolder lineNo' versionNo' cd rd = do
    Just (lc :: LineComment) <- decode <$> LB.readFile (commentFolder </> show lineNo')
    let Just ind = elemIndex cd ((versions lc) !! versionNo')
        newcd = CommentDesc (cuserIdent cd) (cdateTime cd) (cstatus cd)
                  (comment cd) (replies cd ++ [rd])
        splc = splitAt versionNo' $ versions lc
        spvn = splitAt ind ((versions lc) !! versionNo')
        newvn = fst spvn ++ (newcd : (tail $ snd spvn))
        newlc = LineComment lineNo' $ fst splc ++ (newvn : (tail $ snd splc))
    LB.writeFile (commentFolder </> show lineNo') $ encode newlc

deleteCommentFromFile :: FilePath -> Int -> Int -> CommentDesc -> IO ()
deleteCommentFromFile commentFolder lineNo' versionNo' cd = do
    Just (lc :: LineComment) <- decode <$> LB.readFile (commentFolder </> show lineNo')
    let Just ind = elemIndex cd ((versions lc) !! versionNo')
        newcd = CommentDesc "none" (cdateTime cd) "deleted" "none" (replies cd)
        splc = splitAt versionNo' $ versions lc
        spvn = splitAt ind ((versions lc) !! versionNo')
        newvn = fst spvn ++ (if (length $ replies cd) /= 0
                            then newcd : (tail $ snd spvn)
                            else tail $ snd spvn)
        newlc = LineComment lineNo' $ fst splc ++ (newvn : (tail $ snd splc))
    LB.writeFile (commentFolder </> show lineNo') $ encode newlc

deleteReplyFromComment :: FilePath -> Int -> Int -> CommentDesc -> ReplyDesc -> IO ()
deleteReplyFromComment commentFolder lineNo' versionNo' cd rd = do
    Just (lc :: LineComment) <- decode <$> LB.readFile (commentFolder </> show lineNo')
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
    LB.writeFile (commentFolder </> show lineNo') $ encode newlc
