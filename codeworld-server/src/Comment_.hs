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
import qualified Data.ByteString.Lazy as LB
import           Data.List
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
    , ("getUserIdent",            getUserIdent clientId)
    , ("listComments",            listCommentsHandler clientId)
    , ("listOwnerComments",       listOwnerCommentsHandler clientId)
    , ("listOwnerVersions",       listOwnerVersionsHandler clientId)
    , ("listUnreadComments",      listUnreadCommentsHandler clientId)
    , ("listUnreadOwnerComments", listUnreadOwnerCommentsHandler clientId)
    , ("listVersions",            listVersionsHandler clientId)
    , ("readComment",             readCommentHandler clientId)
    , ("readOwnerComment",        readOwnerCommentHandler clientId)
    , ("viewCommentSource",       viewCommentSourceHandler clientId)
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
            commentFolder = commentRootDir mode (userId user) finalDir projectId
        case length path' of
          0 -> return (user, mode, commentFolder)
          _ -> case path' !! 0 of
            x | x /= "commentables" -> return (user, mode, commentFolder)
      2 -> do
        Just commentHash <- fmap (CommentId . T.decodeUtf8) <$> getParam "chash"
        commentFolder <- liftIO $
          BC.unpack <$> B.readFile (commentHashRootDir mode </> commentHashLink commentHash)
        return (user, mode, commentFolder)
      _ -> do
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
      decode <$> LB.readFile (commentHashPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
      Just ind -> do
        let userIdent' = uuserIdent (currentUsers !! ind)
        case userIdent' == cuserIdent comment' of
          True -> liftIO $ do
            deleteCommentFromFile commentFolder lineNo' versionNo' comment'
          False -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 500
            writeBS . BC.pack $ "User Identifier Not Allowed To Delete This Comment"
      Nothing -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $ "User Identifier Not Found"

deleteOwnerCommentHandler :: ClientId -> Snap ()
deleteOwnerCommentHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 1 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    case T.pack "Anonymous Owner" == cuserIdent comment' of
      True -> liftIO $ do
        deleteCommentFromFile commentFolder lineNo' versionNo' comment'
      False -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $ "User Identifier Not Allowed To Delete This Comment"

deleteOwnerReplyHandler :: ClientId -> Snap ()
deleteOwnerReplyHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 1 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    Just (reply' :: ReplyDesc) <- (decodeStrict =<<) <$> getParam "reply"
    case T.pack "Anonymous Owner" == ruserIdent reply' of
      True -> liftIO $ do
        deleteReplyFromComment commentFolder lineNo' versionNo' comment' reply'
      False -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $ "User Identifier Not Allowed To Delete This Reply"

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
      decode <$> LB.readFile (commentHashPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
      Just ind -> do
        let userIdent' = uuserIdent (currentUsers !! ind)
        case userIdent' == cuserIdent comment' of
          True -> liftIO $ do
            deleteReplyFromComment commentFolder lineNo' versionNo' comment' reply'
          False -> do
            modifyResponse $ setContentType "text/plain"
            modifyResponse $ setResponseCode 500
            writeBS . BC.pack $ "User Identifier Not Allowed To Delete This Reply"
      Nothing -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $ "User Identifier Not Found"

getUserIdent :: ClientId -> Snap ()
getUserIdent clientId = do
    (user, mode, commentFolder) <- getFrequentParams 3 clientId
    let commentHash = nameToCommentHash commentFolder
        commentHashPath = commentHashRootDir mode </> commentHashLink commentHash
    Just (currentUsers :: [UserDump]) <- liftIO $
      decode <$> LB.readFile (commentHashPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
      Just ind -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . T.encodeUtf8 . uuserIdent $ currentUsers !! ind
      Nothing -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
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
      decode <$> LB.readFile (commentHashPath <.> "users")
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
    (_, _, commentFolder) <- getFrequentParams 1 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    unreadComments <- liftIO $ listUnreadComments "Anonymous Owner" commentFolder versionNo'
    modifyResponse $ setContentType "application/json"
    writeLBS . encode $ unreadComments

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
      decode <$> LB.readFile (commentHashPath <.> "users")
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
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $ "User Identifier Not Found"

readOwnerCommentHandler :: ClientId -> Snap ()
readOwnerCommentHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 1 clientId
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    comments' <- liftIO $ getLineComment commentFolder lineNo' versionNo'
    liftIO $ markReadComments "Anonymous Owner" commentFolder lineNo' versionNo'
    modifyResponse $ setContentType "application/json"
    writeLBS (encode comments')

viewCommentSourceHandler :: ClientId -> Snap ()
viewCommentSourceHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 3 clientId
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
      decode <$> LB.readFile (commentHashPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
      Just ind -> liftIO $ do
        let userIdent' = uuserIdent (currentUsers !! ind)
            commentDesc = CommentDesc userIdent' dateTime' "present" comment' []
        addCommentToFile commentFolder lineNo' versionNo' commentDesc
        markUnreadComments userIdent' commentFolder lineNo' versionNo'
      Nothing -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $ "User Identifier Not Found"

writeOwnerCommentHandler :: ClientId -> Snap ()
writeOwnerCommentHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 1 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: Text) <- fmap (T.decodeUtf8) <$> getParam "comment"
    Just (dateTime' :: UTCTime) <- (decodeStrict =<<) <$> getParam "dateTime"
    let commentDesc = CommentDesc "Anonymous Owner" dateTime' "present" comment' []
    liftIO $ do
        addCommentToFile commentFolder lineNo' versionNo' commentDesc
        markUnreadComments "Anonymous Owner" commentFolder lineNo' versionNo'

writeOwnerReplyHandler :: ClientId -> Snap ()
writeOwnerReplyHandler clientId = do
    (_, _, commentFolder) <- getFrequentParams 1 clientId
    Just (versionNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "versionNo"
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    Just (reply' :: Text) <- fmap (T.decodeUtf8) <$> getParam "reply"
    Just (dateTime' :: UTCTime) <- (decodeStrict =<<) <$> getParam "dateTime"
    let replyDesc = ReplyDesc "Anonymous Owner" dateTime' "present" reply'
    liftIO $ do
        addReplyToComment commentFolder lineNo' versionNo' comment' replyDesc

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
      decode <$> LB.readFile (commentHashPath <.> "users")
    let currentUserIds = map uuserId currentUsers
    case (userId user) `elemIndex` currentUserIds of
      Just ind -> liftIO $ do
        let userIdent' = uuserIdent (currentUsers !! ind)
            replyDesc = ReplyDesc userIdent' dateTime' "present" reply'
        addReplyToComment commentFolder lineNo' versionNo' comment' replyDesc
      Nothing -> do
        modifyResponse $ setContentType "text/plain"
        modifyResponse $ setResponseCode 500
        writeBS . BC.pack $ "User Identifier Not Found"
