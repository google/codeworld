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

module Comment_ where

import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.List
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import           Snap.Core
import           System.Directory
import           System.FilePath

import CommentUtil
import DataUtil
import Model
import SnapUtil

commentRoutes :: ClientId -> [(B.ByteString, Snap ())]
commentRoutes clientId =
    [ ("addSharedComment",        addSharedCommentHandler clientId)
    , ("commentShare",            commentShareHandler clientId)
    , ("deleteComment",           deleteCommentHandler clientId)
    , ("deleteOwnerComment",      deleteOwnerCommentHandler clientId)
    , ("deleteOwnerReply",        deleteOwnerReplyHandler clientId)
    , ("deleteReply",             deleteReplyHandler clientId)
    , ("getUserIdent",            getUserIdentHandler clientId)
    , ("getOwnerUserIdent",       getOwnerUserIdentHandler clientId)
    , ("listComments",            listCommentsHandler clientId)            -- to be integrated
    , ("listOwnerComments",       listOwnerCommentsHandler clientId)       -- to be integrated
    , ("listOwnerVersions",       listOwnerVersionsHandler clientId)
    , ("listUnreadComments",      listUnreadCommentsHandler clientId)      -- to be integrated
    , ("listUnreadOwnerComments", listUnreadOwnerCommentsHandler clientId) -- to be integrated
    , ("listVersions",            listVersionsHandler clientId)
    , ("readComment",             readCommentHandler clientId)
    , ("readOwnerComment",        readOwnerCommentHandler clientId)
    , ("viewCommentSource",       viewCommentSourceHandler clientId)
    , ("viewOwnerCommentSource",  viewOwnerCommentSourceHandler clientId)
    , ("writeComment",            writeCommentHandler clientId)
    , ("writeOwnerComment",       writeOwnerCommentHandler clientId)
    , ("writeOwnerReply",         writeOwnerReplyHandler clientId)
    , ("writeReply",              writeReplyHandler clientId)
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
            commentFolder <- liftIO $ (<.> "comments") . BC.unpack <$> B.readFile file
            case length path' of
                0 -> return (user, mode, commentFolder)
                _ -> case path' !! 0 of
                         x | x /= "commentables" -> return (user, mode, commentFolder)
        2 -> do
            Just commentHash <- fmap (CommentId . T.decodeUtf8) <$> getParam "chash"
            commentFolder <- liftIO $
              BC.unpack <$> B.readFile (commentHashRootDir mode </> commentHashLink commentHash)
            return (user, mode, commentFolder)
        3 -> do
            Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
            Just name <- getParam "name"
            let projectId = nameToProjectId $ T.decodeUtf8 name
                cDir = joinPath $ map (dirBase . nameToDirId . T.pack) $ tail path'
            case path' !! 0 of
                "commentables" -> liftIO $ do
                    commentHashFile <- BC.unpack <$> B.readFile
                      (sharedCommentsDir mode (userId user) </> cDir </> commentProjectLink projectId)
                    commentFolder <- BC.unpack <$> B.readFile commentHashFile
                    return (user, mode, commentFolder)
        _ -> do
            Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
            Just name <- getParam "name"
            let projectId = nameToProjectId $ T.decodeUtf8 name
                finalDir = joinPath $ map (dirBase . nameToDirId . T.pack) path'
                file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
            case length path' of
                0 -> return (user, mode, file)
                _ -> case path' !! 0 of
                         x | x /= "commentables" -> return (user, mode, file)

addSharedCommentHandler :: ClientId -> Snap ()
addSharedCommentHandler clientId = do
    (user, mode, commentFolder) <- getFrequentParams 2 clientId
    Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    case path' !! 0 of
        "commentables" -> do
            liftIO $ ensureUserProjectDir mode (userId user)
            liftIO $ ensureSharedCommentsDir mode (userId user)
            Just name <- getParam "name"
            Just userIdent' <- fmap (T.decodeUtf8) <$> getParam "userIdent"
            let pathDir = joinPath $ map (dirBase . nameToDirId . T.pack) $ tail path'
                projectId = nameToProjectId $ T.decodeUtf8 name
                finalDir = sharedCommentsDir mode (userId user) </> pathDir
                commentHash = nameToCommentHash commentFolder
            res <- liftIO $ do
                addNewUser (userId user) userIdent' (BC.unpack name)
                  (finalDir </> commentProjectLink projectId)
                  (commentHashRootDir mode </> commentHashLink commentHash)
            case res of
                Left err -> do
                    modifyResponse $ setContentType "text/plain"
                    modifyResponse $ setResponseCode 404
                    writeBS . BC.pack $ err
                Right _ -> return ()
        _ -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "Shared Comments Should Be In `commentables` Directory"

commentShareHandler :: ClientId -> Snap ()
commentShareHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 1 clientId
    modifyResponse $ setContentType "text/plain"
    writeBS . T.encodeUtf8 . unCommentId . nameToCommentHash $ commentFolder

deleteCommentHandler :: ClientId -> Snap ()
deleteCommentHandler clientId = do
    (user, mode, commentFolder) <- getFrequentParams 3 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (commentHashPath <.> "users")
    let (currUserInd :: Int) = fromJust $ userId user `elemIndex` (map uuserId currentUsers)
        currentUser = currentUsers !! currUserInd
    case (uuserId currentUser == userId user,
          uuserIdent currentUser == cuserIdent comment',
          utype currentUser == "not_owner") of
        (True, True, True) -> liftIO $ do
            deleteCommentFromFile commentFolder lineNo' versionNo' comment'
        (True, False, True) -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Allowed To Delete This Comment"
        (_, _, _) -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

deleteOwnerCommentHandler :: ClientId -> Snap ()
deleteOwnerCommentHandler clientId = do
    (user, mode, commentFolder) <- getFrequentParams 1 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    let commentHashPath = commentHashRootDir mode </> commentHashLink (nameToCommentHash commentFolder)
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (commentHashPath <.> "users")
    let (currUserInd :: Int) = fromJust $ userId user `elemIndex` (map uuserId currentUsers)
        currentUser = currentUsers !! currUserInd
    case (uuserId currentUser == userId user,
          uuserIdent currentUser == cuserIdent comment',
          utype currentUser == "owner") of
        (True, True, True) -> liftIO $ do
            deleteCommentFromFile commentFolder lineNo' versionNo' comment'
        (True, False, True) -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Allowed To Delete This Comment"
        (_, _, _) -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

deleteOwnerReplyHandler :: ClientId -> Snap ()
deleteOwnerReplyHandler clientId = do
    (user, mode, commentFolder) <- getFrequentParams 1 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    Just (reply' :: ReplyDesc) <- (decodeStrict =<<) <$> getParam "reply"
    let commentHashPath = commentHashRootDir mode </> commentHashLink (nameToCommentHash commentFolder)
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (commentHashPath <.> "users")
    let (currUserInd :: Int) = fromJust $ userId user `elemIndex` (map uuserId currentUsers)
        currentUser = currentUsers !! currUserInd
    case (uuserId currentUser == userId user,
          uuserIdent currentUser == ruserIdent reply',
          utype currentUser == "owner") of
        (True, True, True) -> liftIO $ do
            deleteReplyFromComment commentFolder lineNo' versionNo' comment' reply'
        (True, False, True) -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Allowed To Delete This Reply"
        (_, _, _) -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

deleteReplyHandler :: ClientId -> Snap ()
deleteReplyHandler clientId = do
    (user, mode, commentFolder) <- getFrequentParams 3 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    Just (reply' :: ReplyDesc) <- (decodeStrict =<<) <$> getParam "reply"
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (commentHashPath <.> "users")
    let (currUserInd :: Int) = fromJust $ userId user `elemIndex` (map uuserId currentUsers)
        currentUser = currentUsers !! currUserInd
    case (uuserId currentUser == userId user,
          uuserIdent currentUser == ruserIdent reply',
          utype currentUser == "not_owner") of
        (True, True, True) -> liftIO $ do
            deleteReplyFromComment commentFolder lineNo' versionNo' comment' reply'
        (True, False, True) -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Allowed To Delete This Comment"
        (_, _, _) -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

getUserIdentHandler :: ClientId -> Snap ()
getUserIdentHandler clientId = do
    (user, mode, commentFolder) <- getFrequentParams 3 clientId
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (commentHashPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
        Just ind -> do
            modifyResponse $ setContentType "text/plain"
            writeBS . T.encodeUtf8 . uuserIdent $ currentUsers !! ind
        Nothing -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

getOwnerUserIdentHandler :: ClientId -> Snap ()
getOwnerUserIdentHandler clientId = do
    (user, _, commentFolder) <- getFrequentParams 1 clientId
    let projectPath = take (length commentFolder - 9) commentFolder
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (projectPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
        Just ind -> do
            modifyResponse $ setContentType "text/plain"
            writeBS . T.encodeUtf8 . uuserIdent $ currentUsers !! ind
        Nothing -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

listCommentsHandler :: ClientId -> Snap ()
listCommentsHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 3 clientId
    modifyResponse $ setContentType "application/json"
    writeLBS =<< (liftIO $ encode <$> listDirectory commentFolder)

listOwnerCommentsHandler :: ClientId -> Snap ()
listOwnerCommentsHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 1 clientId
    modifyResponse $ setContentType "application/json"
    writeLBS =<< (liftIO $ encode <$> listDirectory commentFolder)

listOwnerVersionsHandler :: ClientId -> Snap ()
listOwnerVersionsHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 1 clientId
    modifyResponse $ setContentType "application/json"
    writeLBS =<< (liftIO $ encode <$> listDirectory (commentFolder <.> "versions"))

listUnreadCommentsHandler :: ClientId -> Snap ()
listUnreadCommentsHandler clientId = do
    (user, mode, commentFolder) <- getFrequentParams 3 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (commentHashPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
        Just ind -> do
            let userIdent' = uuserIdent (currentUsers !! ind)
            unreadComments <- liftIO $ listUnreadComments userIdent' commentFolder versionNo'
            modifyResponse $ setContentType "application/json"
            writeLBS . encode $ unreadComments
        Nothing -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 500
            writeBS . BC.pack $ "User Identifier Not Found"

listUnreadOwnerCommentsHandler :: ClientId -> Snap ()
listUnreadOwnerCommentsHandler clientId = do
    (user, _, commentFolder) <- getFrequentParams 1 clientId
    let projectPath = take (length commentFolder - 9) commentFolder
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (projectPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
        Just ind -> do
            Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
            unreadComments <- liftIO $ listUnreadComments
              (uuserIdent $ currentUsers !! ind) commentFolder versionNo'
            modifyResponse $ setContentType "application/json"
            writeLBS . encode $ unreadComments
        Nothing -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

listVersionsHandler :: ClientId -> Snap ()
listVersionsHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 3 clientId
    modifyResponse $ setContentType "application/json"
    writeLBS =<< (liftIO $ encode <$> listDirectory (commentFolder <.> "versions"))

readCommentHandler :: ClientId -> Snap ()
readCommentHandler clientId = do
    (user, mode, commentFolder) <- getFrequentParams 3  clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (commentHashPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
        Just ind -> do
            let userIdent' = uuserIdent (currentUsers !! ind)
            comments' <- liftIO $ getLineComment commentFolder lineNo' versionNo'
            liftIO $ markReadComments userIdent' commentFolder lineNo' versionNo'
            modifyResponse $ setContentType "application/json"
            writeLBS (encode comments')
        Nothing -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

readOwnerCommentHandler :: ClientId -> Snap ()
readOwnerCommentHandler clientId = do
    (user, _, commentFolder) <- getFrequentParams 1 clientId
    let projectPath = take (length commentFolder - 9) commentFolder
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (projectPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
        Just ind -> do
            Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
            Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
            comments' <- liftIO $ getLineComment commentFolder lineNo' versionNo'
            liftIO $ markReadComments (uuserIdent $ currentUsers !! ind)
              commentFolder lineNo' versionNo'
            modifyResponse $ setContentType "application/json"
            writeLBS (encode comments')
        Nothing -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

viewCommentSourceHandler :: ClientId -> Snap ()
viewCommentSourceHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 3 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    currentSource <- liftIO $ B.readFile (commentFolder <.> "versions" </> show versionNo')
    modifyResponse $ setContentType "text/x-haskell"
    writeBS currentSource

viewOwnerCommentSourceHandler :: ClientId -> Snap()
viewOwnerCommentSourceHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 1 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    currentSource <- liftIO $ B.readFile (commentFolder <.> "versions" </> show versionNo')
    modifyResponse $ setContentType "text/x-haskell"
    writeBS currentSource

writeCommentHandler :: ClientId -> Snap ()
writeCommentHandler clientId = do
    (user, mode, commentFolder) <- getFrequentParams 3 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: Text) <- fmap (T.decodeUtf8) <$> getParam "comment"
    Just (dateTime' :: UTCTime) <- (decodeStrict =<<) <$> getParam "dateTime"
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (commentHashPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
        Just ind -> liftIO $ do
            let userIdent' = uuserIdent (currentUsers !! ind)
                commentDesc = CommentDesc userIdent' dateTime' "present" comment' []
            addCommentToFile commentFolder lineNo' versionNo' commentDesc
            markUnreadComments userIdent' commentFolder lineNo' versionNo'
        Nothing -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Does Not Exists"

writeOwnerCommentHandler :: ClientId -> Snap ()
writeOwnerCommentHandler clientId = do
    (user, _, commentFolder) <- getFrequentParams 1 clientId
    let projectPath = take (length commentFolder - 9) commentFolder
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (projectPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
        Just ind -> do
            Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
            Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
            Just (comment' :: Text) <- fmap (T.decodeUtf8) <$> getParam "comment"
            Just (dateTime' :: UTCTime) <- (decodeStrict =<<) <$> getParam "dateTime"
            let userIdent' = uuserIdent $ currentUsers !! ind
                commentDesc = CommentDesc userIdent' dateTime' "present" comment' []
            liftIO $ do
                addCommentToFile commentFolder lineNo' versionNo' commentDesc
                markUnreadComments userIdent' commentFolder lineNo' versionNo'
        Nothing -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

writeOwnerReplyHandler :: ClientId -> Snap ()
writeOwnerReplyHandler clientId = do
    (user, _, commentFolder) <- getFrequentParams 1 clientId
    let projectPath = take (length commentFolder - 9) commentFolder
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (projectPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
        Just ind -> do
            Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
            Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
            Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
            Just (reply' :: Text) <- fmap (T.decodeUtf8) <$> getParam "reply"
            Just (dateTime' :: UTCTime) <- (decodeStrict =<<) <$> getParam "dateTime"
            let userIdent' = uuserIdent $ currentUsers !! ind
                replyDesc = ReplyDesc userIdent' dateTime' "present" reply'
            liftIO $ do
                addReplyToComment commentFolder lineNo' versionNo' comment' replyDesc
        Nothing -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 404
            writeBS . BC.pack $ "User Identifier Not Found"

writeReplyHandler :: ClientId -> Snap ()
writeReplyHandler clientId = do
    (user, mode, commentFolder) <- getFrequentParams 3 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    Just (reply' :: Text) <- fmap (T.decodeUtf8) <$> getParam "reply"
    Just (dateTime' :: UTCTime) <- (decodeStrict =<<) <$> getParam "dateTime"
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- liftIO $
      decodeStrict <$> B.readFile (commentHashPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
      Just ind -> liftIO $ do
        let userIdent' = uuserIdent (currentUsers !! ind)
            replyDesc = ReplyDesc userIdent' dateTime' "present" reply'
        addReplyToComment commentFolder lineNo' versionNo' comment' replyDesc
      Nothing -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 404
        writeBS . BC.pack $ "User Identifier Not Found"
