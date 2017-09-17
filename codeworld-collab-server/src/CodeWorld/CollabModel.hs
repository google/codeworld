{-# LANGUAGE DeriveGeneric #-}
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

module CodeWorld.CollabModel where

import qualified Control.Concurrent.STM as STM
import           Control.OperationalTransformation.Selection (Selection)
import           Control.OperationalTransformation.Server (ServerState)
import           Control.OperationalTransformation.Text (TextOperation)
import           Data.Aeson
import           GHC.Generics (Generic)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)

data CollabServerState = CollabServerState
    { collabProjects :: STM.TVar CollabProjects
    , started        :: UTCTime
    }

type CollabProjects = HM.HashMap CollabId (STM.TVar CollabProject)

data CollabProject = CollabProject
    { totalUsers  :: !Int
    , collabKey   :: CollabId
    , collabState :: ServerState Text TextOperation
    , users       :: [CollabUserState]
    }

data CollabUserState = CollabUserState
    { suserId       :: !Text
    , suserIdent    :: !Text
    , userSelection :: !Selection
    }

instance ToJSON CollabUserState where
    toJSON (CollabUserState _ userIdent' sel) =
      object $ [ "name" .= userIdent' ] ++ (if sel == mempty then [] else [ "selection" .= sel ])

newtype CollabId = CollabId { unCollabId :: Text } deriving (Eq, Generic)

instance Hashable CollabId
