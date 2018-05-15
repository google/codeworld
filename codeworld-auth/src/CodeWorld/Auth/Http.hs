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
Helpers for working with Snap: helpers for parsing "Authorization" headers
as well as for generating various types of response body
-}

{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Auth.Http
    ( badRequest400
    , forbidden403
    , internalServerError500
    , json
    , ok200
    , ok200Json
    , parseBasicAuthHeader
    , parseBearerAuthHeader
    , unauthorized401
    , unauthorized401Json
    ) where

import           CodeWorld.Account (Password(..), UserId(..))
import           Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson (encode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64 (decode)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)
import           Data.List.Split (splitOn)
import qualified Data.Text as Text (pack)
import           Snap.Core
                    ( Response
                    , Snap
                    , emptyResponse
                    , modifyResponse
                    , setContentType
                    , setHeader
                    , setResponseCode
                    , writeLBS
                    )
import           Web.JWT (JSON)

empty :: Int -> Response
empty code = setResponseCode code emptyResponse

ok200 :: Response
ok200 = empty 200

unauthorized401 :: ByteString -> Response
unauthorized401 authType = setHeader "WWW-Authenticate" authType $ empty 401

forbidden403 :: Response
forbidden403 = empty 403

badRequest400 :: Response
badRequest400 = empty 400

internalServerError500 :: Response
internalServerError500 = empty 500

json :: ToJSON a => Int -> a -> Snap ()
json code obj = do
    modifyResponse $ setResponseCode code . setContentType "application/json"
    writeLBS $ Aeson.encode obj

ok200Json :: ToJSON a => a -> Snap ()
ok200Json = json 200

unauthorized401Json :: ToJSON a => ByteString -> a -> Snap ()
unauthorized401Json authType obj = do
    modifyResponse $ setHeader "WWW-Authenticate" authType
    json 401 obj

parseBasicAuthHeader :: ByteString -> Maybe (UserId, Password)
parseBasicAuthHeader bs = do
    t <- parseAuthValue "Basic" bs
    case Base64.decode (Char8.pack t) of
        Left _ -> Nothing
        Right s ->
            case splitOn ":" (Char8.unpack s) of
                [userIdRaw, passwordRaw] -> Just (UserId userIdRaw, Password passwordRaw)
                _ -> Nothing

parseBearerAuthHeader :: ByteString -> Maybe JSON
parseBearerAuthHeader bs = do
    t <- parseAuthValue "Bearer" bs
    return $ Text.pack t

parseAuthValue :: String -> ByteString -> Maybe String
parseAuthValue authType bs =
    case splitOn " " (Char8.unpack bs) of
        [authType', t] -> if authType /= authType' then Nothing else Just t
        _ -> Nothing
