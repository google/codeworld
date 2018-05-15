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

{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Account.Actions
    ( createAccount
    , deleteAccount
    , storeExists
    , fetchAllAccounts
    , initStore
    , updateAccount
    , verifyAccount
    ) where

import qualified CodeWorld.Account.Hashing as Hashing
import           CodeWorld.Account.Types
import           Control.Monad.Trans.State.Strict
                    ( State
                    , execState
                    , modify
                    )
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text (intercalate, pack)
import           Database.SQLite.Simple
                    ( Connection
                    , NamedParam(..)
                    , Query(..)
                    , SQLData(..)
                    , execute
                    , execute_
                    , executeNamed
                    , query_
                    , queryNamed
                    , withConnection
                    )
import           Database.SQLite.Simple.ToField (ToField(..))
import           System.Directory (doesFileExist)

type QueryParams = [(Text, SQLData)]
type QueryParamsBuilder = State QueryParams ()

withStore :: Store -> (Connection -> IO a) -> IO a
withStore (Store dbPath) = withConnection dbPath

storeExists :: Store -> IO Bool
storeExists (Store dbPath) = doesFileExist dbPath

initStore :: Store -> IO ()
initStore store = withStore store $ \conn -> do
    execute_ conn "DROP TABLE IF EXISTS accounts"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS accounts ( \
        \id INTEGER PRIMARY KEY NOT NULL, \
        \userId TEXT NOT NULL UNIQUE, \
        \status TEXT NOT NULL, \
        \passwordHash TEXT NOT NULL \
        \)"

createAccount :: Store -> UserId -> Status -> PasswordHash -> IO ()
createAccount store (UserId userIdRaw) status (PasswordHash passwordHashRaw) =
    withStore store $ \conn ->
        execute conn
            "INSERT INTO accounts (userId, status, passwordHash) VALUES (?, ?, ?)"
            (userIdRaw, status, passwordHashRaw)

updateAccount :: Store -> UserId -> Maybe Status -> Maybe PasswordHash -> IO ()
updateAccount store (UserId userIdRaw) mbStatus mbPasswordHash =
    let params = buildParams $ do
                                addParam mbStatus "status" toField
                                addParam mbPasswordHash "passwordHash" (\(PasswordHash passwordHashRaw) -> SQLBlob passwordHashRaw)
        (q, ps) = renderInsert "accounts" "userId" (SQLText $ Text.pack userIdRaw) params
    in case ps of
        [_] -> pure ()
        _ -> withStore store $ \conn -> executeNamed conn q ps

deleteAccount :: Store -> UserId -> IO ()
deleteAccount store (UserId userIdRaw) =
    withStore store $ \conn ->
        executeNamed conn "DELETE FROM accounts WHERE userId = :userId" [ ":userId" := userIdRaw ]

fetchAllAccounts :: Store -> IO [(UserId, Status)]
fetchAllAccounts store =
    withStore store $ \conn -> do
        result <- query_ conn "SELECT userId, status FROM accounts"
        return $ map (\(userIdRaw, status) -> (UserId userIdRaw, status)) result

verifyAccount :: Store -> UserId -> Password -> IO (Maybe Status)
verifyAccount store (UserId userIdRaw) password = do
    mbResult <- fetch
    case mbResult of
        Nothing -> pure Nothing
        Just (status, passwordHash) ->
            if Hashing.validate passwordHash password
                then pure $ Just status
                else pure Nothing
    where
        fetch :: IO (Maybe (Status, PasswordHash))
        fetch =
            withStore store $ \conn -> do
                result <- queryNamed conn
                            "SELECT status, passwordHash FROM accounts WHERE userId = :userId"
                            [ ":userId" := userIdRaw ]
                case result of
                    [ (status, passwordHashRaw) ] -> return $ Just (status, PasswordHash passwordHashRaw)
                    [] -> return Nothing
                    _ -> error "Assertion failure"

buildParams :: QueryParamsBuilder -> QueryParams
buildParams = (flip execState) []

addParam :: Maybe a -> Text -> (a -> SQLData) -> QueryParamsBuilder
addParam Nothing _ _ = pure ()
addParam (Just value) name f =
    modify
        (\items -> (name, f value) : items)

renderInsert :: Text -> Text -> SQLData -> QueryParams -> (Query, [NamedParam])
renderInsert tableName keyName keyValue params =
    let assignments = map (\(name, _) -> name <> " = :" <> name) params
        ps = (":" <> keyName := keyValue) : map (\(name, value) -> ":" <> name := value) params
    in (Query $ "UPDATE "
            <> tableName
            <> " SET "
            <> Text.intercalate ", " assignments
            <> " WHERE " <> keyName <> " = :" <> keyName, ps)
