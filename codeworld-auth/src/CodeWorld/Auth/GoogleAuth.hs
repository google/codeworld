{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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
Snap routes and functions for default Google authentication method
-}

{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Auth.GoogleAuth
    ( AuthConfig
    , authRoutes
    , authenticated
    , configureAuth
    , optionallyAuthenticated
    ) where

import           CodeWorld.Account (UserId(..))
import           CodeWorld.Auth.Http
import           CodeWorld.Auth.Types
import           Control.Monad (mzero, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
                    ( (.:)
                    , FromJSON
                    , Value(..)
                    , decode
                    , parseJSON
                    )
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8 (unpack)
import           Data.Text (Text)
import qualified Data.Text as Text (strip)
import qualified Data.Text.IO as Text (readFile)
import           Network.HTTP.Conduit (simpleHttp)
import           Snap.Core (Snap, finishWith, getParam, pass)
import           System.Directory (doesFileExist)
import           System.FilePath ((</>))

newtype ClientId = ClientId Text deriving Eq

data AuthConfig = AuthConfig ClientId

data User = User UserId Text

instance FromJSON User where
    parseJSON (Object v) = User <$> (UserId <$> v .: "user_id") <*> v .: "audience"
    parseJSON _ = mzero

configureAuth :: FilePath -> IO (Maybe AuthConfig)
configureAuth appDir = do
    let clientIdPath = appDir </> "web/clientId.txt"
    clientIdPathExists <- doesFileExist clientIdPath
    case clientIdPathExists of
        False -> do
            putStrLn $
                "Client ID file not found at " ++
                clientIdPath ++
                ": skipping configuration of Google authentication"
            pure Nothing
        True -> do
            clientIdContent <- Text.readFile "web/clientId.txt"
            let clientId = ClientId $ Text.strip clientIdContent
            pure $ Just (AuthConfig clientId)

authRoutes :: AuthConfig -> [Route]
authRoutes _ = []

authenticated :: (UserId -> Snap ()) -> AuthConfig -> Snap ()
authenticated handler authConfig = do
    mbIdToken <- getParam "id_token"
    case mbIdToken of
        Nothing -> finishWith forbidden403
        Just idToken -> do
            userId  <- authenticatedHelper authConfig idToken
            handler userId

optionallyAuthenticated :: (Maybe UserId -> Snap ()) -> AuthConfig -> Snap ()
optionallyAuthenticated handler authConfig = do
    mbIdToken <- getParam "id_token"
    case mbIdToken of
        Nothing -> handler Nothing
        Just idToken -> do
            userId  <- authenticatedHelper authConfig idToken
            handler (Just userId)

-- |Helper for authenticated API calls: retrieves the user for the current
-- request. The request should have an @id_token@ parameter with an ID token
-- retrieved from the Google authentication API. The user is returned if the
-- ID token is valid.
authenticatedHelper :: AuthConfig -> ByteString -> Snap UserId
authenticatedHelper (AuthConfig clientId) idToken = do
    let url = "https://www.googleapis.com/oauth2/v1/tokeninfo?id_token=" ++ Char8.unpack idToken
    result <- fmap decode $ liftIO $ simpleHttp url
    case result of
        Nothing -> pass
        Just (User userId audience) -> do
            when (clientId /= ClientId audience) pass
            return userId
