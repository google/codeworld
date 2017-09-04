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

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Strict (toList)
import           Data.Text (Text, pack)
import           Data.Time.Clock (UTCTime)

data User = User { userId :: Text, audience :: Text }

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "user_id"
                                <*> v .: "audience"
    parseJSON _          = mzero

data Project = Project {
    projectSource :: Text,
    projectHistory :: Value
    }

instance FromJSON Project where
    parseJSON (Object v) = Project <$> v .: "source"
                                   <*> v .: "history"
    parseJSON _          = mzero

instance ToJSON Project where
    toJSON p = object [ "source"  .= projectSource p,
                        "history" .= projectHistory p ]

data Directory = Directory {
    files :: [Text],
    dirs :: [Text]
    }

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
    rstatus :: Text,
    reply :: Text
    } deriving (Eq)

instance FromJSON ReplyDesc where
    parseJSON (Object o) = ReplyDesc <$> o .: "userIdent"
                                     <*> o .: "dateTime"
                                     <*> o .: "status"
                                     <*> o .: "reply"
    parseJSON _ = mzero

instance ToJSON ReplyDesc where
    toJSON rd = object [ "userIdent" .= ruserIdent rd,
                         "dateTime"  .= rdateTime rd,
                         "status"    .= rstatus rd, --present or deleted
                         "reply"     .= reply rd ]

data CommentDesc = CommentDesc {
    cuserIdent :: Text,
    cdateTime :: UTCTime,
    cstatus :: Text,
    comment :: Text,
    replies :: [ReplyDesc]
    } deriving (Eq)

instance FromJSON CommentDesc where
    parseJSON (Object o) = CommentDesc <$> o .: "userIdent"
                                       <*> o .: "dateTime"
                                       <*> o .: "status"
                                       <*> o .: "comment"
                                       <*> o .: "replies"
    parseJSON _ = mzero

instance ToJSON CommentDesc where
    toJSON cd = object [ "userIdent" .= cuserIdent cd,
                         "dateTime"  .= cdateTime cd,
                         "status"    .= cstatus cd,
                         "comment"   .= comment cd,
                         "replies"   .= replies cd ]

data LineComment = LineComment {
    lineNo :: Int, -- 0 for global
    versions :: [[CommentDesc]]
    }

instance FromJSON LineComment where
    parseJSON (Object o) = LineComment <$> o .: "lineNo"
                                       <*> o .: "versions"
    parseJSON _ = mzero

instance ToJSON LineComment where
    toJSON lc = object [ "lineNo"   .= lineNo lc,
                         "versions" .= versions lc ]

data LineStatus = LineStatus {
    llineNo :: Int,
    lstatus :: Text -- "read" or "unread"
    }

newtype LineStatuses = LineStatuses { listStatuses :: [LineStatus] }

instance FromJSON LineStatuses where
    parseJSON x = LineStatuses <$> (parseJSON x >>= mapM parseLineStatus . toList)

parseLineStatus :: (String, Value) -> Parser LineStatus
parseLineStatus (k, v) = LineStatus (read k :: Int) <$> parseJSON v

instance ToJSON LineStatuses where
    toJSON lss = object $
                   map (\ls -> (pack . show $ llineNo ls) .= lstatus ls) $ listStatuses lss

data UserDump = UserDump {
    uuserId :: Text,
    uuserIdent :: Text,
    upath :: Text,
    utype :: Text
    } deriving (Eq)

instance FromJSON UserDump where
    parseJSON (Object o) = UserDump <$> o .: "userId"
                                    <*> o .: "userIdent"
                                    <*> o .: "path"
                                    <*> o .: "type"
    parseJSON _ = mzero

instance ToJSON UserDump where
    toJSON ud = object [ "userId"    .= uuserId ud,
                         "userIdent" .= uuserIdent ud,
                         "path"      .= upath ud,
                         "type"      .= utype ud ]

data VersionLS = VersionLS {
    versionNo :: Int,
    versionStatus :: LineStatuses
    }

newtype VersionLS_ = VersionLS_ { getVersionLS :: [VersionLS] }

instance FromJSON VersionLS_ where
    parseJSON x = VersionLS_ <$> (parseJSON x >>= mapM parseVersionLS . toList)

parseVersionLS :: (String, Value) -> Parser VersionLS
parseVersionLS (k, v) = VersionLS (read k :: Int) <$> parseJSON v

instance ToJSON VersionLS_ where
    toJSON vls = object $
                   map (\x -> (pack . show $ versionNo x) .= versionStatus x) $ getVersionLS vls
