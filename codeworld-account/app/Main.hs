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

module Main (main) where

import           CodeWorld.Account
import           Control.Applicative ((<|>), optional)
import           Control.Monad (replicateM, when, zipWithM)
import qualified Data.ByteString.Lazy.Char8 as Char8 (putStrLn)
import           Data.Csv (ToRecord(..), encode, toField, record)
import           Data.List (delete)
import           Data.Monoid ((<>))
import           Options.Applicative
                    ( Parser
                    , argument
                    , command
                    , execParser
                    , flag'
                    , fullDesc
                    , header
                    , help
                    , helper
                    , info
                    , long
                    , maybeReader
                    , metavar
                    , option
                    , progDesc
                    , short
                    , strOption
                    , subparser
                    , switch
                    )
import           System.Exit (exitFailure)
import           System.Random (randomRIO)

data DumpRow = DumpRow UserId Status

instance ToRecord DumpRow where
    toRecord (DumpRow (UserId userId) status) =
        record [toField userId, toField $ show status]

data Options = Options Store Command

data Command =
    Init Bool
    | Dump
    | Create UserId Status
    | Update UserId (Maybe Status) PasswordUpdate
    | Delete UserId
    | Verify UserId Password

data PasswordUpdate = GeneratePassword | SetPassword Password | NoPasswordUpdate

pOptions :: Parser Options
pOptions = Options <$> pStore <*> pCommand

pStore :: Parser Store
pStore = option (maybeReader (Just . Store)) $
    long "db"
    <> short 'd'
    <> metavar "DBPATH"
    <> help "Database path"

pCommand :: Parser Command
pCommand = subparser
    ( command "init" (info pInit (progDesc "Initialize database"))
    <> command "dump" (info (pure Dump) (progDesc "Dump database"))
    <> command "create" (info pCreate (progDesc "Create account"))
    <> command "update" (info pUpdate (progDesc "Update account"))
    <> command "delete" (info pDelete (progDesc "Delete account"))
    <> command "verify" (info pVerify (progDesc "Verify password"))
    )

pInit :: Parser Command
pInit = Init <$> (switch $ long "force" <> short 'f' <> help "Force overwrite of existing database")

pCreate :: Parser Command
pCreate = Create <$> pUserId <*> pStatus

pUpdate :: Parser Command
pUpdate = Update
    <$> pUserId
    <*> (optional $ option (maybeReader readStatus) (long "status" <> short 's' <> help "Account status"))
    <*> (pGeneratePassword <|> pSetPassword <|> pure NoPasswordUpdate)

pGeneratePassword :: Parser PasswordUpdate
pGeneratePassword = flag' GeneratePassword (long "generate-password" <> short 'g' <> help "Generate new password")

pSetPassword :: Parser PasswordUpdate
pSetPassword = SetPassword . Password <$> strOption (long "password" <> short 'p' <> help "Set password")

pDelete :: Parser Command
pDelete = Delete <$> pUserId

pVerify :: Parser Command
pVerify = Verify <$> pUserId <*> pPassword

readStatus :: String -> Maybe Status
readStatus s
    | s == "active" = Just Active
    | s == "expired" = Just Expired
    | otherwise = Nothing

pUserId :: Parser UserId
pUserId = argument (maybeReader (Just . UserId)) (metavar "USERID")

pStatus :: Parser Status
pStatus = argument (maybeReader readStatus) (metavar "STATUS")

pPassword :: Parser Password
pPassword = argument (maybeReader (Just . Password)) (metavar "PASSWORD")

main :: IO ()
main = parse >>= go
    where
        parse = execParser $ info (helper <*> pOptions) $
            fullDesc
            <> header "codeworld-account"
            <> progDesc "CodeWorld account maintenance tool"
        go (Options store (Init overwrite)) = doInit store overwrite
        go (Options store Dump) = doDump store
        go (Options store (Create userId status)) = doCreate store userId status
        go (Options store (Update userId mbStatus passwordUpdate)) = doUpdate store userId mbStatus passwordUpdate
        go (Options store (Delete userId)) = doDelete store userId
        go (Options store (Verify userId password)) = doVerify store userId password

reportError :: String -> IO a
reportError m = putStrLn m >> exitFailure

doInit :: Store -> Bool -> IO ()
doInit store overwrite = do
    when (not overwrite) $ do
        exists <- storeExists store
        when exists (reportError "Account store already exists")
    initStore store

doDump :: Store -> IO ()
doDump store = do
    accounts <- fetchAllAccounts store
    Char8.putStrLn (encode $ map (uncurry DumpRow) accounts)

doCreate :: Store -> UserId -> Status -> IO ()
doCreate store userId status = do
    password <- makeDefaultPassword
    passwordHash <- hash password
    createAccount store userId status passwordHash
    putStrLn $ "Created new account with password " ++ show password


doUpdate :: Store -> UserId -> Maybe Status -> PasswordUpdate -> IO ()
doUpdate store userId mbStatus NoPasswordUpdate = updateAccount store userId mbStatus Nothing
doUpdate store userId mbStatus GeneratePassword = do
    password <- makeDefaultPassword
    passwordHash <- hash password
    updateAccount store userId mbStatus (Just passwordHash)
    putStrLn $ "Updated existing account with password " ++ show password
doUpdate store userId mbStatus (SetPassword password) = do
    passwordHash <- hash password
    updateAccount store userId mbStatus (Just passwordHash)
    putStrLn "Updated existing account with specified password"

doDelete :: Store -> UserId -> IO ()
doDelete store userId = deleteAccount store userId

doVerify :: Store -> UserId -> Password -> IO ()
doVerify store userId password = do
    mbAccount <- verifyAccount store userId password
    case mbAccount of
        Nothing -> putStrLn "Account could not be verified"
        Just _ -> putStrLn "Account verified"

makeDefaultPassword :: IO Password
makeDefaultPassword = Password
    <$> makePasswordString
        [ "abcdefghijkmnpqrstuvwxyz"    -- Deliberately exclude "l" and "o"
        , ['0'..'9']
        ]
        7

makePasswordString :: [String] -> Int -> IO String
makePasswordString charSets n = do
    parts <- getPartition n
    chars <- zipWithM replicateM parts (pick <$> charSets)
    shuffle (concat chars)
    where
        getPartition :: Int -> IO [Int]
        getPartition n' = adjust <$> replicateM (k - 1) (randomRIO (1, n' `div` k))

        k :: Int
        k = length charSets

        adjust :: [Int] -> [Int]
        adjust p = (n - sum p) : p

        shuffle :: Eq a => [a] -> IO [a]
        shuffle [] = pure []
        shuffle items = do
            x <- pick items
            xs <- shuffle (delete x items)
            return (x : xs)

        pick :: [a] -> IO a
        pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)
