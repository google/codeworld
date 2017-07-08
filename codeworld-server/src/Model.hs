{-# LANGUAGE OverloadedStrings #-}

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

module Model where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           System.FilePath (FilePath)

data User = User { userId :: Text, audience :: Text }

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "user_id"
                                <*> v .: "audience"
    parseJSON _          = mzero

data Project = Project {
    projectName :: Text,
    projectSource :: Text,
    projectHistory :: Value
    }

instance FromJSON Project where
    parseJSON (Object v) = Project <$> v .: "name"
                                   <*> v .: "source"
                                   <*> v .: "history"
    parseJSON _          = mzero

instance ToJSON Project where
    toJSON p = object [ "name"    .= projectName p,
                        "source"  .= projectSource p,
                        "history" .= projectHistory p ]

data Directory = Directory {
    files :: [Text],
    dirs :: [Text]
    } deriving Show

instance ToJSON Directory where
    toJSON dir = object [ "files" .= files dir,
                          "dirs"  .= dirs dir ]

data CompileResult = CompileResult {
    compileHash :: Text,
    compileDeployHash :: Text
    }

instance ToJSON CompileResult where
    toJSON cr = object [ "hash"  .= compileHash cr,
                         "dhash" .= compileDeployHash cr ]

data ReplyDesc = ReplyDesc {
    ruserIdent :: Text,
    rdateTime :: UTCTime,
    reply :: Text
    } deriving (Eq)

instance FromJSON ReplyDesc where
    parseJSON (Object o) = ReplyDesc <$> o .: "userIdent"
                                     <*> o .: "dateTime"
                                     <*> o .: "reply"
    parseJSON _ = mzero

instance ToJSON ReplyDesc where
    toJSON rd = object [ "userIdent" .= ruserIdent rd,
                         "dateTime"  .= rdateTime rd,
                         "reply"     .= reply rd ]

data CommentDesc = CommentDesc {
    userIdent :: Text,
    dateTime :: UTCTime,
    comment :: Text,
    replies :: [ReplyDesc]
    } deriving (Eq)

instance FromJSON CommentDesc where
    parseJSON (Object o) = CommentDesc <$> o .: "userIdent"
                                       <*> o .: "dateTime"
                                       <*> o .: "comment"
                                       <*> o .: "replies"
    parseJSON _ = mzero

instance ToJSON CommentDesc where
    toJSON cd = object [ "userIdent" .= userIdent cd,
                         "dateTime"  .= dateTime cd,
                         "comment"   .= comment cd,
                         "replies"   .= replies cd ]

data LineComment = LineComment {
    lineNo :: Int, -- 0 for global
    comments :: [CommentDesc]
    }

instance FromJSON LineComment where
    parseJSON (Object o) = LineComment <$> o .: "lineNo"
                                       <*> o .: "comments"
    parseJSON _ = mzero

instance ToJSON LineComment where
    toJSON lc = object [ "lineNo"    .= lineNo lc,
                         "comments" .= comments lc ]
