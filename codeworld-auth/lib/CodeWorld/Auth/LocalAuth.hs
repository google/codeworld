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

module CodeWorld.Auth.LocalAuth
    ( AuthConfig
    , authRoutes
    , authenticated
    , configureAuth
    , optionallyAuthenticated
    ) where

import           CodeWorld.Account
                    ( Password(..)
                    , Status(..)
                    , Store(..)
                    , UserId(..)
                    , hash
                    , storeExists
                    , updateAccount
                    , verifyAccount
                    )
import           CodeWorld.Auth.Http
import           CodeWorld.Auth.Secret
import           CodeWorld.Auth.Time
import           CodeWorld.Auth.Util
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (throwE)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)
import           Data.List.Split (splitOn)
import           Data.Text (Text)
import qualified Data.Text as Text (pack, unpack)
import           Data.Time.Clock
                    ( NominalDiffTime
                    , UTCTime
                    , addUTCTime
                    , getCurrentTime
                    )
import           Prelude hiding (exp)
import           Snap.Core
                    ( Method(..)
                    , Snap
                    , finishWith
                    , getHeader
                    , getParam
                    , getRequest
                    , method
                    , modifyResponse
                    , pass
                    , setHeader
                    )
import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           Web.JWT
                    ( JSON
                    , JWTClaimsSet(..)
                    , Signer(..)
                    , claims
                    , decodeAndVerifySignature
                    , encodeSigned
                    , stringOrURI
                    , stringOrURIToText
                    )

data AuthConfig = AuthConfig Signer Store

jwtIssuer :: Text
jwtIssuer = "https://code.world/"

-- JWT token expires after an hour
jwtExpiryDuration :: NominalDiffTime
jwtExpiryDuration = fromInteger 3600

jwtSigner :: Secret -> Signer
jwtSigner (Secret bytes) = HMACSecret bytes

configureAuth :: FilePath -> IO (Maybe AuthConfig)
configureAuth appDir = do
    let secretPath = appDir </> "local-auth-secret.txt"
        storePath = appDir </> "local-auth.db"
    secretPathExists <- doesFileExist secretPath
    case secretPathExists of
        False -> do
            putStrLn $ "Secret key file not found at " ++ secretPath ++ ": skipping configuration of local authentication"
            pure Nothing
        True -> do
            signer <- jwtSigner <$> readSecret secretPath
            let store = Store storePath
            storeExists_ <- storeExists store
            case storeExists_ of
                False -> do
                    putStrLn $ "Account store database file not found at " ++ storePath ++ ": skipping configuration of local authentication"
                    pure Nothing
                True -> pure $ Just (AuthConfig signer store)

authRoutes :: AuthConfig -> [(ByteString, Snap ())]
authRoutes authConfig =
    [ ("signIn", method POST (signInHandler authConfig))
    ]

authenticated :: (UserId -> Snap ()) -> AuthConfig -> Snap ()
authenticated handler authConfig = do
    req <- getRequest
    case getHeader "Authorization" req of
        Nothing -> finishWith unauthorized401
        Just authHeaderBS -> authenticatedHelper authConfig handler authHeaderBS

optionallyAuthenticated :: (Maybe UserId -> Snap ()) -> AuthConfig -> Snap ()
optionallyAuthenticated handler authConfig = do
    req <- getRequest
    case getHeader "Authorization" req of
        Nothing -> handler Nothing
        Just authHeaderBS -> authenticatedHelper authConfig (\userId -> handler $ Just userId) authHeaderBS

authenticatedHelper :: AuthConfig -> (UserId -> Snap ()) -> ByteString -> Snap ()
authenticatedHelper authConfig@(AuthConfig signer _) handler authHeaderBS = withSnapExcept $ do
    (issuer, expiresAt, userId) <- hoistMaybe (finishWith forbidden403) $ do
        t <- parseAuthHeader authHeaderBS
        jwt <- decodeAndVerifySignature signer t
        let c = claims jwt
        issuer' <- iss c
        expiresAt' <- exp c
        userIdStr' <- (UserId . Text.unpack . stringOrURIToText) <$> sub c
        return (issuer', expiresAt', userIdStr')

    when
        (stringOrURIToText issuer /= jwtIssuer)
        (throwE $ finishWith forbidden403)

    now <- liftIO getCurrentTime
    when
        (now >= numericDateToUTCTime expiresAt)
        (throwE (unauthorized401Json $ m [ ("reason", "token-expired") ]))

    lift $ do
        handler userId
        addAuthHeader authConfig userId

authHeader :: JSON -> ByteString
authHeader t = Char8.pack $ "Bearer " ++ (Text.unpack t)

parseAuthHeader :: ByteString -> Maybe JSON
parseAuthHeader bs =
    case splitOn " " (Char8.unpack bs) of
        [authType, t] -> if authType /= "Bearer"
                            then Nothing
                            else Just (Text.pack t)
        _ -> Nothing

addAuthHeader :: AuthConfig -> UserId -> Snap ()
addAuthHeader (AuthConfig signer _) userId = do
    issuedAt <- liftIO getCurrentTime
    case jwtToken signer userId issuedAt jwtExpiryDuration of
        Nothing -> pass
        Just t -> modifyResponse $ setHeader "Authorization" (authHeader t)

jwtToken :: Signer -> UserId -> UTCTime -> NominalDiffTime -> Maybe JSON
jwtToken signer (UserId userIdStr) issuedAt expiryDuration = do
    let expiresAt = addUTCTime expiryDuration issuedAt
    issuedAtNum <- utcTimeToNumericDate issuedAt
    expiresAtNum <- utcTimeToNumericDate expiresAt
    let claimsSet = mempty
            { iss = stringOrURI jwtIssuer
            , sub = stringOrURI (Text.pack userIdStr)
            , exp = Just expiresAtNum
            , iat = Just issuedAtNum
            }
    return $ encodeSigned signer claimsSet

signInHandler :: AuthConfig -> Snap ()
signInHandler authConfig = do
    userId <- UserId . Char8.unpack <$> getRequiredParam "userId"
    password <- Password . Char8.unpack <$> getRequiredParam "password"
    mbNewPassword <- fmap (Password . Char8.unpack) <$> getParam "newPassword"
    case mbNewPassword of
        Nothing -> signIn authConfig userId password
        Just newPassword -> signInWithNewPassword authConfig userId password newPassword

signIn :: AuthConfig -> UserId -> Password -> Snap ()
signIn authConfig@(AuthConfig _ store) userId password = do
    mbStatus <- liftIO $ verifyAccount store userId password
    case mbStatus of
        Nothing -> finishWith forbidden403
        Just Active -> addAuthHeader authConfig userId
        Just Expired -> unauthorized401Json $ m [ ("reason", "password-expired") ]

signInWithNewPassword :: AuthConfig -> UserId -> Password -> Password -> Snap ()
signInWithNewPassword authConfig@(AuthConfig _ store) userId password newPassword = do
    when
        (not $ meetsPasswordPolicy password newPassword)
        (finishWith badRequest400)

    mbStatus <- liftIO $ verifyAccount store userId password
    case mbStatus of
        Nothing -> finishWith forbidden403
        Just _ -> do
            liftIO $ do
                passwordHash <- hash newPassword
                updateAccount store userId (Just Active) (Just passwordHash)
            addAuthHeader authConfig userId

meetsPasswordPolicy :: Password -> Password -> Bool
meetsPasswordPolicy (Password passwordRaw) (Password newPasswordRaw)
    | passwordRaw == newPasswordRaw = False
    | length newPasswordRaw < 5 = False
    | otherwise = True
