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
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import           Data.List (elemIndex, splitAt)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory
import           System.FilePath
import           System.IO

import Model
import Util

newtype CommentId = CommentId { unCommentId :: Text } deriving Eq

commentHashRootDir :: BuildMode -> FilePath
commentHashRootDir (BuildMode m) = "data" </> m </> "commentHash"

commentRootDir :: BuildMode -> Text -> FilePath -> ProjectId -> FilePath
commentRootDir mode userId path projectId = userProjectDir mode userId </> path </> projectFile projectId <.> "comments"

commentHashLink :: CommentId -> FilePath
commentHashLink (CommentId c) = let s = T.unpack c in take 3 s </> s

nameToCommentHash :: FilePath -> CommentId
nameToCommentHash = CommentId . hashToId "C" . BC.pack

ensureCommentHashDir :: BuildMode -> CommentId -> IO ()
ensureCommentHashDir mode (CommentId c) = createDirectoryIfMissing True dir
  where dir = commentHashRootDir mode </> take 3 (T.unpack c)

getLineComment :: FilePath -> Int -> IO (Maybe LineComment)
getLineComment commentFolder lineNo = do
    fileBool <- doesFileExist (commentFolder </> show lineNo)
    case fileBool of
      True  -> decode <$> LB.readFile (commentFolder </> show lineNo)
      False -> return (Just $ LineComment lineNo [])

addCommentToFile :: FilePath -> Int -> CommentDesc -> IO ()
addCommentToFile commentFolder lineNo comment = do
    fileBool <- doesFileExist (commentFolder </> show lineNo)
    lc <- case fileBool of
      True  -> do
        Just (lc :: LineComment) <- decode <$> LB.readFile (commentFolder </> show lineNo)
        return lc
      False -> return (LineComment lineNo [])
    LB.writeFile (commentFolder </> show lineNo) $ encode (LineComment lineNo $ comments lc ++ [comment])

addReplyToComment :: FilePath -> Int -> CommentDesc -> ReplyDesc -> IO ()
addReplyToComment commentFolder lineNo cd rd = do
    Just (lc :: LineComment) <- decode <$> LB.readFile (commentFolder </> show lineNo)
    let Just ind = elemIndex cd (comments lc)
        newcd = CommentDesc (userIdent cd) (dateTime cd) (comment cd) (replies cd ++ [rd])
        splc = splitAt ind $ comments lc
        newlc = LineComment lineNo $ (fst splc) ++ (newcd : (tail $ snd splc))
    LB.writeFile (commentFolder </> show lineNo) $ encode newlc

deleteCommentFromFile :: FilePath -> Int -> CommentDesc -> IO ()
deleteCommentFromFile commentFolder lineNo cd = do
    Just (lc :: LineComment) <- decode <$> LB.readFile (commentFolder </> show lineNo)
    let Just ind = elemIndex cd (comments lc)
        newcd = CommentDesc "none" (dateTime cd) "deleted" (replies cd)
        splc = splitAt ind $ comments lc
        newlc = LineComment lineNo $ (fst splc) ++ (if (length $ replies cd) /= 0 
                                                    then newcd : (tail $ snd splc)
                                                    else tail $ snd splc)
    LB.writeFile (commentFolder </> show lineNo) $ encode newlc

deleteReplyFromComment :: FilePath -> Int -> CommentDesc -> ReplyDesc -> IO ()
deleteReplyFromComment commentFolder lineNo cd rd = do
    Just (lc :: LineComment) <- decode <$> LB.readFile (commentFolder </> show lineNo)
    let Just cdInd = elemIndex cd (comments lc)
        Just rdInd = elemIndex rd (replies cd)
        splc = splitAt cdInd $ comments lc
        spcd = splitAt rdInd $ replies cd
        newcd = CommentDesc (userIdent cd) (dateTime cd) (comment cd) $ (fst spcd) ++ (tail $ snd spcd)
        newlc = LineComment lineNo $ (fst splc) ++ (newcd : (tail $ snd splc))
    LB.writeFile (commentFolder </> show lineNo) $ encode newlc
