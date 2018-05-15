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
Snap routes and functions for local authentication method
-}

{-
    = Authentication workflow

    This is a high-level description of the authentication workflow
    implemented by functions in this module.

    == Signing in to the service

    Request:
      POST /signIn
      Headers: [ ("Authorization", "Basic BASE64(user:password)") ]
      Body: n/a

    If user name and password are valid and account active:
      Status: 200
      Body: { accessToken: AAA, refreshToken: RRR }

    If account expired:
      Status: 403
      Body: { reason: "account-expired" }

    Otherwise:
      Status: 403
      Body: n/a

    == Subsequent API calls

    Request:
      DELETE|GET|POST|PUT /some-api
      Headers: [ ("Authorization", "Bearer AAA") ]
      Body: { /* request body */ }

    If access token expired:
      Status: 403
      Body: { reason: "token-expired" }

    Otherwise:
      Status: /* status code */
      Body: { /* response body */ }

    == Obtaining a new refresh token

    Request:
      POST /refreshToken
      Body: { refreshToken: RRR }

    If refresh token is valid and not expired:
      Status: 200
      Body: { accessToken: AAA', refreshToken: RRR' }

    Otherwise:
      Status: 403
      Body: n/a

    == Signing out from the service

    Request:
      POST /signOut
      Body: { refreshToken: RRR' }

    On success:
      Status: 200
      Body: n/a

    Otherwise:
      Status: /* status code */
      Body: { /* response body */ }
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
                    , incrementTokenId
                    , storeExists
                    , updateAccount
                    , verifyPassword
                    , verifyTokenId
                    )
import           CodeWorld.Auth.Http
import           CodeWorld.Auth.Secret
import           CodeWorld.Auth.Token
import           CodeWorld.Auth.Util
import           Control.Monad (when, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (throwE)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8 (unpack)
import qualified Data.Text as Text (pack, unpack)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Snap.Core
                    ( Method(..)
                    , Snap
                    , finishWith
                    , getHeader
                    , getParam
                    , getRequest
                    , method
                    )
import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           Web.JWT (Signer(..))

data AuthConfig = AuthConfig Signer Store

codeWorldIssuer :: Issuer
codeWorldIssuer = Issuer "https://code.world/"

jwtSigner :: Secret -> Signer
jwtSigner (Secret bytes) = HMACSecret bytes

jwtAuthType :: ByteString
jwtAuthType = "Bearer"

configureAuth :: FilePath -> IO (Maybe AuthConfig)
configureAuth appDir = do
    let secretPath = appDir </> "codeworld-auth.txt"
        storePath = appDir </> "codeworld-auth.db"
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
    [ ("refreshToken", method POST (refreshTokenHandler authConfig))
    , ("signIn", method POST (signInHandler authConfig))
    , ("signOut", method POST (signOutHandler authConfig))
    ]

authenticated :: (UserId -> Snap ()) -> AuthConfig -> Snap ()
authenticated handler authConfig = do
    req <- getRequest
    case getHeader "Authorization" req of
        Nothing -> finishWith (unauthorized401 jwtAuthType)
        Just authHeaderBS -> authenticatedHelper authConfig handler authHeaderBS True

optionallyAuthenticated :: (Maybe UserId -> Snap ()) -> AuthConfig -> Snap ()
optionallyAuthenticated handler authConfig = do
    req <- getRequest
    case getHeader "Authorization" req of
        Nothing -> handler Nothing
        Just authHeaderBS -> authenticatedHelper authConfig (\userId -> handler $ Just userId) authHeaderBS True

authenticatedHelper :: AuthConfig -> (UserId -> Snap ()) -> ByteString -> Bool -> Snap ()
authenticatedHelper (AuthConfig signer _) handler authHeaderBS checkExpiry = withSnapExcept $ do
    AccessToken issuer _ expiresAt userId <- hoistMaybe (finishWith forbidden403) $ do
        j <- parseBearerAuthHeader authHeaderBS
        parseAccessToken signer j

    when
        (issuer /= codeWorldIssuer)
        (throwE $ finishWith forbidden403)

    when checkExpiry $ do
        now <- liftIO getCurrentTime
        when
            (now >= expiresAt)
            (throwE (unauthorized401Json jwtAuthType $ m [ ("reason", "token-expired") ]))

    lift $ handler userId

refreshTokenHandler :: AuthConfig -> Snap ()
refreshTokenHandler authConfig@(AuthConfig signer store) = do
    j <- Text.pack . Char8.unpack <$> getRequiredParam "refreshToken"
    withSnapExcept $ do
        -- 1. Parse refresh token
        RefreshToken issuer _ expiresAt userId tokenId <- hoistMaybe (finishWith forbidden403) $ parseRefreshToken signer j

        -- 2. Check issuer
        when
            (issuer /= codeWorldIssuer)
            (throwE $ finishWith forbidden403)

        -- 3. Check expiry
        now <- liftIO getCurrentTime
        when
            (now >= expiresAt)
            (throwE (unauthorized401Json jwtAuthType $ m [ ("reason", "token-expired") ]))

        -- 4. Check token ID
        isValidTokenId <- liftIO $ verifyTokenId store userId tokenId
        when (not isValidTokenId) (throwE $ finishWith forbidden403)

        -- 5. Generate response containing new access and refresh tokens
        lift $ generateTokenJson authConfig userId now

signInHandler :: AuthConfig -> Snap ()
signInHandler authConfig@(AuthConfig _ store) = withSnapExcept $ do
    req <- lift getRequest
    (userId, password) <- hoistMaybe (finishWith (unauthorized401 jwtAuthType)) $ do
        authHeader <- getHeader "Authorization" req
        parseBasicAuthHeader authHeader

    mbNewPassword <- fmap (Password . Char8.unpack) <$> (lift $ getParam "newPassword")
    lift $ case mbNewPassword of
        -- Regular sign-in
        Nothing -> do
            mbStatus <- liftIO $ verifyPassword store userId password
            case mbStatus of
                Nothing -> finishWith forbidden403
                Just Active -> do
                    now <- liftIO getCurrentTime
                    generateTokenJson authConfig userId now
                Just Expired -> passwordExpiredJson

        -- New password specified
        Just newPassword -> do
            when
                (not $ satisfiesPasswordPolicy password newPassword)
                (finishWith badRequest400)

            mbStatus <- liftIO $ verifyPassword store userId password
            case mbStatus of
                Nothing -> finishWith forbidden403
                Just _ -> do
                    now <- liftIO $ do
                        passwordHash <- hash newPassword
                        updateAccount store userId (Just Active) (Just passwordHash)
                        getCurrentTime
                    generateTokenJson authConfig userId now

signOutHandler :: AuthConfig -> Snap ()
signOutHandler (AuthConfig signer store) = do
    j <- Text.pack . Char8.unpack <$> getRequiredParam "refreshToken"
    withSnapExcept $ do
        RefreshToken issuer _ _ userId tokenId <- hoistMaybe (finishWith forbidden403) $ parseRefreshToken signer j

        when
            (issuer /= codeWorldIssuer)
            (throwE $ finishWith forbidden403)

        -- Check token validity and increment the token ID to invalidate any existing refresh tokens
        liftIO $ do
            isValid <- verifyTokenId store userId tokenId
            when isValid (void $ incrementTokenId store userId)

        lift $ finishWith ok200

generateTokenJson :: AuthConfig -> UserId -> UTCTime -> Snap ()
generateTokenJson (AuthConfig signer store) userId now = withSnapExcept $ do
    -- 1. Generate new token ID
    mbNewTokenId <- liftIO $ incrementTokenId store userId
    newTokenId <- hoistMaybe
                    (finishWith internalServerError500)
                    mbNewTokenId

    -- 2. Generate new access and refresh tokens
    let at = accessToken codeWorldIssuer now userId
    atJson <- hoistMaybe
                (finishWith internalServerError500)
                (renderAccessToken signer at)
    let rt = refreshToken codeWorldIssuer now userId newTokenId
    rtJson <- hoistMaybe
                (finishWith internalServerError500)
                (renderRefreshToken signer rt)

    -- 7. HTTP 200 response with tokens
    lift $ ok200Json $ m
        [ ("accessToken", Text.unpack atJson)
        , ("refreshToken", Text.unpack rtJson)
        ]

satisfiesPasswordPolicy :: Password -> Password -> Bool
satisfiesPasswordPolicy (Password passwordRaw) (Password newPasswordRaw)
    | passwordRaw == newPasswordRaw = False
    | length newPasswordRaw < 5 = False
    | otherwise = True

passwordExpiredJson :: Snap ()
passwordExpiredJson = unauthorized401Json jwtAuthType $ m [ ("reason", "password-expired") ]