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

-- |
-- Functions an types for working with JWT bearer tokens
module CodeWorld.Auth.Token
  ( AccessToken (..),
    Issuer (..),
    RefreshToken (..),
    accessToken,
    parseAccessToken,
    parseRefreshToken,
    refreshToken,
    renderAccessToken,
    renderRefreshToken,
  )
where

import CodeWorld.Account (TokenId (..), UserId (..))
import CodeWorld.Auth.Time
import Data.Aeson (Value (..))
import Data.Map (Map)
import qualified Data.Map as Map (fromList, lookup)
import Data.Text (Text)
import qualified Data.Text as Text (pack, unpack)
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime,
    addUTCTime,
  )
import Text.Read (readMaybe)
import Web.JWT
  ( ClaimsMap (..),
    JWTClaimsSet (..),
    Signer (..),
    StringOrURI,
    claims,
    decodeAndVerifySignature,
    encodeSigned,
    stringOrURI,
    stringOrURIToText,
    unClaimsMap,
  )
import Prelude hiding (exp)

newtype Issuer = Issuer Text deriving (Eq, Show)

data TokenType = Access | Refresh deriving (Eq, Show)

data AccessToken = AccessToken Issuer UTCTime UTCTime UserId

data RefreshToken = RefreshToken Issuer UTCTime UTCTime UserId TokenId

-- | Access token expiry period: access tokens vended by local auth
--  system (via /signIn, /refreshToken) will expire after this time
--  period: clients must then request a new access and refresh token
--  using the /refreshToken API.
accessTokenExpiryPeriod :: NominalDiffTime
accessTokenExpiryPeriod = seconds 60

-- | Refresh token expiry period: refresh tokens expire after this
--  time period at which the client is required to reauthenticate the
--  user via the /signIn API. This is loosely equivalent to a
--  traditional "session expiry period".
refreshTokenExpiryPeriod :: NominalDiffTime
refreshTokenExpiryPeriod = minutes 60

toStringOrURI :: Issuer -> Maybe StringOrURI
toStringOrURI (Issuer issuerRaw) = stringOrURI issuerRaw

valueText :: Value -> Maybe Text
valueText (String s) = Just s
valueText _ = Nothing

accessToken :: Issuer -> UTCTime -> UserId -> AccessToken
accessToken issuer issuedAt userId =
  let expiresAt = addUTCTime accessTokenExpiryPeriod issuedAt
   in AccessToken issuer issuedAt expiresAt userId

refreshToken :: Issuer -> UTCTime -> UserId -> TokenId -> RefreshToken
refreshToken issuer issuedAt userId tokenId =
  let expiresAt = addUTCTime refreshTokenExpiryPeriod issuedAt
   in RefreshToken issuer issuedAt expiresAt userId tokenId

renderAccessToken :: Signer -> AccessToken -> Maybe Text
renderAccessToken signer (AccessToken issuer issuedAt expiresAt userId) =
  renderHelper signer issuer issuedAt expiresAt userId $
    Map.fromList [("token-type", String "access")]

renderRefreshToken :: Signer -> RefreshToken -> Maybe Text
renderRefreshToken signer (RefreshToken issuer issuedAt expiresAt userId (TokenId tokenId)) =
  renderHelper signer issuer issuedAt expiresAt userId $
    Map.fromList [("token-type", String "refresh"), ("token-id", String $ (Text.pack . show) tokenId)]

renderHelper :: Signer -> Issuer -> UTCTime -> UTCTime -> UserId -> Map Text Value -> Maybe Text
renderHelper signer issuer issuedAt expiresAt (UserId userIdRaw) extraClaims = do
  issuedAtNum <- utcTimeToNumericDate issuedAt
  expiresAtNum <- utcTimeToNumericDate expiresAt
  let claimsSet =
        mempty
          { iss = toStringOrURI issuer,
            sub = stringOrURI (Text.pack userIdRaw),
            exp = Just expiresAtNum,
            iat = Just issuedAtNum,
            unregisteredClaims = ClaimsMap extraClaims
          }
  return $ encodeSigned signer mempty claimsSet

parseAccessToken :: Signer -> Text -> Maybe AccessToken
parseAccessToken signer j = do
  (tokenType, issuer, issuedAt, expiresAt, userId, _) <- parseHelper signer j
  case tokenType of
    Access -> Just $ AccessToken issuer issuedAt expiresAt userId
    _ -> Nothing

parseRefreshToken :: Signer -> Text -> Maybe RefreshToken
parseRefreshToken signer j = do
  (tokenType, issuer, issuedAt, expiresAt, userId, extraClaims) <- parseHelper signer j
  case tokenType of
    Refresh -> do
      tokenIdValue <- Map.lookup "token-id" extraClaims
      tokenIdRaw <- valueText tokenIdValue
      tokenId <- TokenId <$> readMaybe (Text.unpack tokenIdRaw)
      Just $ RefreshToken issuer issuedAt expiresAt userId tokenId
    _ -> Nothing

parseHelper :: Signer -> Text -> Maybe (TokenType, Issuer, UTCTime, UTCTime, UserId, Map Text Value)
parseHelper signer j = do
  jwt <- decodeAndVerifySignature signer j
  let c = claims jwt
  issuer <- (Issuer . stringOrURIToText) <$> iss c
  issuedAt <- numericDateToUTCTime <$> iat c
  expiresAt <- numericDateToUTCTime <$> exp c
  userId <- (UserId . Text.unpack . stringOrURIToText) <$> sub c
  let extraClaims = unClaimsMap $ unregisteredClaims c
  tokenTypeValue <- Map.lookup "token-type" extraClaims
  tokenTypeRaw <- valueText tokenTypeValue
  tokenType <- parseTokenType tokenTypeRaw
  return (tokenType, issuer, issuedAt, expiresAt, userId, extraClaims)
  where
    parseTokenType s
      | s == "access" = Just Access
      | s == "refresh" = Just Refresh
      | otherwise = Nothing
