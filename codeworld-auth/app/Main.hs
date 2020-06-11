{-
  Copyright 2020 The CodeWorld Authors. All rights reserved.

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
                    ( Password(..)
                    , Status(..)
                    , Store(..)
                    , TokenId(..)
                    , UserId(..)
                    )
import qualified CodeWorld.Account as Account
                    ( hash
                    , createAccount
                    , fetchAllAccounts
                    , incrementTokenId
                    , initStore
                    , storeExists
                    , updateAccount
                    , verifyPassword
                    , verifyTokenId
                    )
import qualified CodeWorld.Auth.Secret as Secret (generateSecret, writeSecret)
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
                    , eitherReader
                    , execParser
                    , flag'
                    , fullDesc
                    , header
                    , help
                    , helper
                    , info
                    , long
                    , metavar
                    , option
                    , progDesc
                    , short
                    , strOption
                    , subparser
                    , switch
                    , value
                    )
import           System.Exit (exitFailure)
import           System.Random (randomRIO)
import           Text.Read (readMaybe)

defaultStore :: Store
defaultStore = Store "codeworld-auth.db"

defaultSecretPath :: FilePath
defaultSecretPath = "codeworld-auth.txt"

data DumpRow = DumpRow UserId Status TokenId

instance ToRecord DumpRow where
    toRecord (DumpRow (UserId userIdRaw) status (TokenId tokenIdRaw)) =
        record
            [ toField userIdRaw
            , toField $ show status
            , toField tokenIdRaw
            ]

data Command =
    InitAccounts Store Bool
    | DumpAccounts Store
    | CreateAccount Store UserId Status
    | UpdateAccount Store UserId (Maybe Status) PasswordUpdate
    | DeleteAccount Store UserId
    | VerifyPassword Store UserId Password
    | IncrementTokenId Store UserId
    | VerifyTokenId Store UserId TokenId
    | GenerateSecret FilePath

data PasswordUpdate = GeneratePassword | SetPassword Password | NoPasswordUpdate

pCommand :: Parser Command
pCommand = subparser
    ( command "init-accounts" (info pInitAccounts (progDesc "Initialize account database"))
    <> command "dump-accounts" (info pDumpAccounts (progDesc "Dump account database"))
    <> command "create-account" (info pCreateAccount (progDesc "Create account"))
    <> command "update-account" (info pUpdateAccount (progDesc "Update account"))
    <> command "delete-account" (info pDeleteAccount (progDesc "Delete account"))
    <> command "verify-password" (info pVerifyPassword (progDesc "Verify password"))
    <> command "increment-token-id" (info pIncrementTokenId (progDesc "Increment (invalidate) token ID"))
    <> command "verify-token-id" (info pVerifyTokenId (progDesc "Verify token ID"))
    <> command "generate-secret" (info pGenerateSecret (progDesc "Generate secret"))
    )

pInitAccounts :: Parser Command
pInitAccounts = InitAccounts
    <$> pStore
    <*> (switch $ long "force" <> short 'f' <> help "Force overwrite of existing database")

pDumpAccounts :: Parser Command
pDumpAccounts = DumpAccounts <$> pStore

pCreateAccount :: Parser Command
pCreateAccount = CreateAccount <$> pStore <*> pUserId <*> pStatus

pUpdateAccount :: Parser Command
pUpdateAccount = UpdateAccount
    <$> pStore
    <*> pUserId
    <*> (optional $ option (eitherReader readStatus) (long "status" <> short 's' <> help "Account status"))
    <*> (pGeneratePassword <|> pSetPassword <|> pure NoPasswordUpdate)

pDeleteAccount :: Parser Command
pDeleteAccount = DeleteAccount <$> pStore <*> pUserId

pVerifyPassword :: Parser Command
pVerifyPassword = VerifyPassword <$> pStore <*> pUserId <*> pPassword

pIncrementTokenId :: Parser Command
pIncrementTokenId = IncrementTokenId <$> pStore <*> pUserId

pVerifyTokenId :: Parser Command
pVerifyTokenId = VerifyTokenId <$> pStore <*> pUserId <*> pTokenId

pGenerateSecret :: Parser Command
pGenerateSecret = GenerateSecret <$> strOption
    ( long "secret-path"
    <> short 's'
    <> help "Specify secret path"
    <> metavar "SECRETPATH"
    <> value defaultSecretPath
    )

readStatus :: String -> Either String Status
readStatus s
    | s == "Active" = Right Active
    | s == "Expired" = Right Expired
    | otherwise = Left $ "Invalid status value \"" ++ s ++ "\""

pStore :: Parser Store
pStore = option
    (eitherReader (Right . Store))
    ( long "db-path"
    <> short 'd'
    <> help "Specify account database path"
    <> metavar "AUTHDBPATH"
    <> value defaultStore
    )

pUserId :: Parser UserId
pUserId = argument (eitherReader (Right . UserId)) (metavar "USERID")

pStatus :: Parser Status
pStatus = argument (eitherReader readStatus) (metavar "STATUS")

pGeneratePassword :: Parser PasswordUpdate
pGeneratePassword = flag' GeneratePassword (long "generate-password" <> short 'g' <> help "Generate new password")

pSetPassword :: Parser PasswordUpdate
pSetPassword = SetPassword . Password <$> strOption (long "password" <> short 'p' <> help "Set password")

pPassword :: Parser Password
pPassword = argument (eitherReader (Right . Password)) (metavar "PASSWORD")

pTokenId :: Parser TokenId
pTokenId = argument (eitherReader readTokenId) (metavar "TOKENID")

readTokenId :: String -> Either String TokenId
readTokenId s = case readMaybe s of
                    Just tokenIdRaw -> Right $ TokenId tokenIdRaw
                    Nothing -> Left $ "Invalid token ID \"" ++ s ++ "\""

main :: IO ()
main = parse >>= go
    where
        parse = execParser $ info (helper <*> pCommand) $
            fullDesc
            <> header "codeworld-auth"
            <> progDesc "CodeWorld account and authentication maintenance tool"
        go (InitAccounts store overwrite) = initAccounts store overwrite
        go (DumpAccounts store) = dumpAccounts store
        go (CreateAccount store userId status) = createAccount store userId status
        go (UpdateAccount store userId mbStatus passwordUpdate) = updateAccount store userId mbStatus passwordUpdate
        go (DeleteAccount store userId) = deleteAccount store userId
        go (VerifyPassword store userId password) = verifyPassword store userId password
        go (IncrementTokenId store userId) = incrementTokenId store userId
        go (VerifyTokenId store userId tokenId) = verifyTokenId store userId tokenId
        go (GenerateSecret path) = generateSecret path

reportError :: String -> IO a
reportError m = putStrLn m >> exitFailure

initAccounts :: Store -> Bool -> IO ()
initAccounts store overwrite = do
    when (not overwrite) $ do
        exists <- Account.storeExists store
        when exists (reportError "Account store already exists")
    Account.initStore store

dumpAccounts :: Store -> IO ()
dumpAccounts store = do
    accounts <- Account.fetchAllAccounts store
    Char8.putStrLn (encode $ map (\(u, s, t) -> DumpRow u s t) accounts)

createAccount :: Store -> UserId -> Status -> IO ()
createAccount store userId status = do
    password@(Password passwordRaw) <- generatePasswordWithDefaultPolicy
    passwordHash <- Account.hash password
    Account.createAccount store userId status passwordHash
    putStrLn $ "Created new account with password " ++ passwordRaw

updateAccount :: Store -> UserId -> Maybe Status -> PasswordUpdate -> IO ()
updateAccount store userId mbStatus NoPasswordUpdate = Account.updateAccount store userId mbStatus Nothing
updateAccount store userId mbStatus GeneratePassword = do
    password <- generatePasswordWithDefaultPolicy
    passwordHash <- Account.hash password
    Account.updateAccount store userId mbStatus (Just passwordHash)
    putStrLn $ "Updated existing account with password " ++ show password
updateAccount store userId mbStatus (SetPassword password) = do
    passwordHash <- Account.hash password
    Account.updateAccount store userId mbStatus (Just passwordHash)
    putStrLn "Updated existing account with specified password"

deleteAccount :: Store -> UserId -> IO ()
deleteAccount store userId = deleteAccount store userId

verifyPassword :: Store -> UserId -> Password -> IO ()
verifyPassword store userId password = do
    mbAccount <- Account.verifyPassword store userId password
    case mbAccount of
        Nothing -> putStrLn "Account could not be verified"
        Just _ -> putStrLn "Account verified"

incrementTokenId :: Store -> UserId -> IO ()
incrementTokenId store userId = do
    mbTokenId <- Account.incrementTokenId store userId
    case mbTokenId of
        Nothing -> putStrLn "Could not increment token ID"
        Just (TokenId tokenIdRaw) -> putStrLn $ "Token ID incremented to " ++ show tokenIdRaw

verifyTokenId :: Store -> UserId -> TokenId -> IO ()
verifyTokenId store userId tokenId = do
    result <- Account.verifyTokenId store userId tokenId
    case result of
        False -> putStrLn "Token ID could not be verified"
        True -> putStrLn "Token ID verified"

generateSecret :: FilePath -> IO ()
generateSecret path = do
    secret_ <- Secret.generateSecret
    Secret.writeSecret path secret_

generatePasswordWithDefaultPolicy :: IO Password
generatePasswordWithDefaultPolicy = Password
    <$> generatePassword
        [ "abcdefghijkmnpqrstuvwxyz"    -- Deliberately exclude "l" and "o"
        , ['0'..'9']
        ]
        7

generatePassword :: [String] -> Int -> IO String
generatePassword charSets n = do
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
