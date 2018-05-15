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

{-|
Types for working with accounts, bearer tokens etc.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Account.Types
    ( Password(..)
    , PasswordHash(..)
    , Status(..)
    , Store(..)
    , TokenId(..)
    , UserId(..)
    ) where

import           Data.ByteString (ByteString)
import           Data.Hashable (Hashable)
import           Database.SQLite.Simple (SQLData(..))
import           Database.SQLite.Simple.FromField (FromField(..), ResultError(..), fieldData, returnError)
import           Database.SQLite.Simple.Ok (Ok(..))
import           Database.SQLite.Simple.ToField (ToField(..))

-- |Configuration, including location on disc, of account database
data Store = Store FilePath

-- |User ID
newtype UserId = UserId String deriving (Eq, Hashable, Show)

-- |Password
newtype Password = Password String deriving (Eq, Show)

-- |Password hash
newtype PasswordHash = PasswordHash ByteString deriving Eq

-- |Monotonically-increasing token identifier used to invalidate
-- refresh tokens issued by bearer-token-based authentication systems
-- including local auth system
newtype TokenId = TokenId Int deriving (Eq, Show)

-- |Account status
data Status =
    Active      -- ^ Active
    | Expired   -- ^ Expired
    deriving Show

instance ToField Status where
    toField Active = SQLText "Active"
    toField Expired = SQLText "Expired"

instance FromField Status where
    fromField f = let d = fieldData f in case d of
                    SQLText "Active" -> Ok Active
                    SQLText "Expired" -> Ok Expired
                    _ -> returnError ConversionFailed f ("Value must be \"Active\" or \"Expired\", got " ++ show d)
