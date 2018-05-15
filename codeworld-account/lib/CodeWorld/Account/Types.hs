{-
  Copyright 2018 The CodeWorld Authors. All rights reserved.
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Account.Types
    ( Password(..)
    , PasswordHash(..)
    , SecretToken(..)
    , Status(..)
    , Store(..)
    , UserId(..)
    ) where

import           Data.ByteString (ByteString)
import           Data.Hashable (Hashable)
import           Database.SQLite.Simple (SQLData(..))
import           Database.SQLite.Simple.FromField (FromField(..), ResultError(..), fieldData, returnError)
import           Database.SQLite.Simple.Ok (Ok(..))
import           Database.SQLite.Simple.ToField (ToField(..))

data Store = Store FilePath
newtype UserId = UserId String deriving (Eq, Hashable, Show)
newtype Password = Password String deriving (Eq, Show)
newtype PasswordHash = PasswordHash ByteString deriving Eq
newtype SecretToken = SecretToken String deriving Show

data Status = Active | Expired deriving Show

instance ToField Status where
    toField Active = SQLText "active"
    toField Expired = SQLText "expired"

instance FromField Status where
    fromField f = let d = fieldData f in case d of
                    SQLText "active" -> Ok Active
                    SQLText "expired" -> Ok Expired
                    _ -> returnError ConversionFailed f ("Value must be \"active\" or \"expired\", got " ++ show d)
