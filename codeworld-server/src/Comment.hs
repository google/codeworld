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

module Comment where

import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           System.Directory
import           System.FilePath

import CommentUtil
import DataUtil
import Model
import SnapUtil

commentRoutes :: ClientId -> [(B.ByteString, Snap ())]
commentRoutes clientId =
    [ ("commentShare",       commentShareHandler clientId)
    , ("deleteComment",      deleteCommentHandler clientId)
    , ("deleteOwnerComment", deleteOwnerCommentHandler clientId)
    , ("deleteOwnerReply",   deleteOwnerReplyHandler clientId)
    , ("deleteReply",        deleteReplyHandler clientId)
    , ("readComment",        readCommentHandler)
    , ("readOwnerComment",   readOwnerCommentHandler clientId)
    , ("viewCommentSource",  viewCommentSourceHandler)
    , ("writeComment",       writeCommentHandler clientId)
    , ("writeOwnerComment",  writeOwnerCommentHandler clientId)
    , ("writeOwnerReply",    writeOwnerReplyHandler clientId)
    , ("writeReply",         writeReplyHandler clientId)
    ]

getFrequentParams :: Bool -> ClientId -> Snap (User, BuildMode, FilePath, Maybe ProjectId)
getFrequentParams owner clientId = do
    user <- getUser clientId
    mode <- getBuildMode
    case owner of
      True -> do
        Just path' <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
        let finalDir = joinPath $ map (dirBase . nameToDirId . T.pack) path'
        Just name <- getParam "name"
        let projectId = nameToProjectId $ T.decodeUtf8 name
        return (user, mode, finalDir, Just projectId)
      False -> do
        Just commentHash <- fmap (CommentId . T.decodeUtf8) <$> getParam "chash"
        commentFolder <- liftIO $
          BC.unpack <$> B.readFile (commentHashRootDir mode </> commentHashLink commentHash)
        return (user, mode, commentFolder, Nothing)

commentShareHandler :: ClientId -> Snap ()
commentShareHandler clientId = do
    (user, mode, finalDir, Just projectId) <- getFrequentParams True clientId
    let commentFolder = commentRootDir mode (userId user) finalDir projectId
    liftIO $ createDirectoryIfMissing False commentFolder
    let commentHash = nameToCommentHash commentFolder
    liftIO $ ensureCommentHashDir mode commentHash
    liftIO $ B.writeFile (commentHashRootDir mode </> commentHashLink commentHash) $
      BC.pack commentFolder
    modifyResponse $ setContentType "text/plain"
    writeBS $ T.encodeUtf8 $ unCommentId commentHash

deleteCommentHandler :: ClientId -> Snap ()
deleteCommentHandler clientId = do
    (_, _, commentFolder, _) <- getFrequentParams False clientId
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    liftIO $ deleteCommentFromFile commentFolder lineNo' comment'

deleteOwnerCommentHandler :: ClientId -> Snap ()
deleteOwnerCommentHandler clientId = do
    (user, mode, finalDir, Just projectId) <- getFrequentParams True clientId
    let commentFolder = commentRootDir mode (userId user) finalDir projectId
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    liftIO $ deleteCommentFromFile commentFolder lineNo' comment'

deleteOwnerReplyHandler :: ClientId -> Snap ()
deleteOwnerReplyHandler clientId = do
    (user, mode, finalDir, Just projectId) <- getFrequentParams True clientId
    let commentFolder = commentRootDir mode (userId user) finalDir projectId
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    Just (reply' :: ReplyDesc) <- (decodeStrict =<<) <$> getParam "reply"
    liftIO $ deleteReplyFromComment commentFolder lineNo' comment' reply'

deleteReplyHandler :: ClientId -> Snap ()
deleteReplyHandler clientId = do
    (_, _, commentFolder, _) <- getFrequentParams False clientId
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    Just (reply' :: ReplyDesc) <- (decodeStrict =<<) <$> getParam "reply"
    liftIO $ deleteReplyFromComment commentFolder lineNo' comment' reply'

readCommentHandler :: Snap ()
readCommentHandler = do
    mode <- getBuildMode
    Just commentHash <- fmap (CommentId . T.decodeUtf8) <$> getParam "chash"
    commentFolder <- liftIO $
      BC.unpack <$> B.readFile (commentHashRootDir mode </> commentHashLink commentHash)
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just comments' <- liftIO $ getLineComment commentFolder lineNo'
    modifyResponse $ setContentType "application/json"
    writeLBS (encode comments')

readOwnerCommentHandler :: ClientId -> Snap ()
readOwnerCommentHandler clientId = do
    (user, mode, finalDir, Just projectId) <- getFrequentParams True clientId
    let commentFolder = commentRootDir mode (userId user) finalDir projectId
    liftIO $ createDirectoryIfMissing False commentFolder
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just comments' <- liftIO $ getLineComment commentFolder lineNo'
    modifyResponse $ setContentType "application/json"
    writeLBS (encode comments')

viewCommentSourceHandler :: Snap ()
viewCommentSourceHandler = do
    mode <- getBuildMode
    Just commentHash <- fmap (CommentId . T.decodeUtf8) <$> getParam "chash"
    commentFolder <- liftIO $
      BC.unpack <$> B.readFile (commentHashRootDir mode </> commentHashLink commentHash)
    Just (project :: Project) <- liftIO $
      decode <$> (LB.readFile $ take (length commentFolder - 9) commentFolder)
    modifyResponse $ setContentType "text/x-haskell"
    writeBS (T.encodeUtf8 $ projectSource project)

writeCommentHandler :: ClientId -> Snap ()
writeCommentHandler clientId = do
    (_, _, commentFolder, _) <- getFrequentParams False clientId
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    liftIO $ addCommentToFile commentFolder lineNo' comment'

writeOwnerCommentHandler :: ClientId -> Snap ()
writeOwnerCommentHandler clientId = do
    (user, mode, finalDir, Just projectId) <- getFrequentParams True clientId
    let commentFolder = commentRootDir mode (userId user) finalDir projectId
    liftIO $ createDirectoryIfMissing False commentFolder
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    liftIO $ addCommentToFile commentFolder lineNo' comment'

writeOwnerReplyHandler :: ClientId -> Snap ()
writeOwnerReplyHandler clientId = do
    (user, mode, finalDir, Just projectId) <- getFrequentParams True clientId
    let commentFolder = commentRootDir mode (userId user) finalDir projectId
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    Just (reply' :: ReplyDesc) <- (decodeStrict =<<) <$> getParam "reply"
    liftIO $ addReplyToComment commentFolder lineNo' comment' reply'

writeReplyHandler :: ClientId -> Snap ()
writeReplyHandler clientId = do
    (_, _, commentFolder, _) <- getFrequentParams False clientId
    Just (lineNo' :: Int) <- fmap (read . BC.unpack) <$> getParam "lineNo"
    Just (comment' :: CommentDesc) <- (decodeStrict =<<) <$> getParam "comment"
    Just (reply' :: ReplyDesc) <- (decodeStrict =<<) <$> getParam "reply"
    liftIO $ addReplyToComment commentFolder lineNo' comment' reply'
