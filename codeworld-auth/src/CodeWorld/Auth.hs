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
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Functions and types for working with various authentication methods
module CodeWorld.Auth
  ( AuthConfig,
    authenticated,
    authMethod,
    authRoutes,
    getAuthConfig,
    optionallyAuthenticated,
  )
where

import CodeWorld.Account (UserId)
import qualified CodeWorld.Auth.GoogleAuth as GoogleAuth
import CodeWorld.Auth.Http
import qualified CodeWorld.Auth.LocalAuth as LocalAuth
import CodeWorld.Auth.Types
import CodeWorld.Auth.Util
import Snap.Core (Snap, finishWith)

-- | Authentication configuration data
data AuthConfig
  = Google GoogleAuth.AuthConfig
  | Local LocalAuth.AuthConfig
  | None

-- | Reads authentication configuration from given application directory
getAuthConfig :: FilePath -> IO AuthConfig
getAuthConfig appDir = do
  mbGoogleAuthConfig <- GoogleAuth.configureAuth appDir
  case mbGoogleAuthConfig of
    Just googleAuthConfig -> pure $ Google googleAuthConfig
    Nothing -> do
      mbLocalAuthConfig <- LocalAuth.configureAuth appDir
      case mbLocalAuthConfig of
        Just localAuthConfig -> pure $ Local localAuthConfig
        Nothing -> pure None

-- | Returns textual representation of authentication method
authMethod :: AuthConfig -> String
authMethod (Google _) = "Google"
authMethod (Local _) = "Local"
authMethod None = "not configured"

-- | Wrapper for Snap handlers that require an authenticated client
authenticated :: (UserId -> Snap ()) -> AuthConfig -> Snap ()
authenticated handler (Local authConfig) = LocalAuth.authenticated handler authConfig
authenticated handler (Google authConfig) = GoogleAuth.authenticated handler authConfig
authenticated _ None = finishWith forbidden403

-- | Wrapper for Snap handlers that can work with authenticated or
--  unauthenticated clients
optionallyAuthenticated :: (Maybe UserId -> Snap ()) -> AuthConfig -> Snap ()
optionallyAuthenticated handler (Local authConfig) = LocalAuth.optionallyAuthenticated handler authConfig
optionallyAuthenticated handler (Google authConfig) = GoogleAuth.optionallyAuthenticated handler authConfig
optionallyAuthenticated handler None = handler Nothing

-- | Provides additional Snap routes specific to authentication method in effect
authRoutes :: AuthConfig -> [Route]
authRoutes authConfig@(Google c) = authMethodRoute authConfig : GoogleAuth.authRoutes c
authRoutes authConfig@(Local c) = authMethodRoute authConfig : LocalAuth.authRoutes c
authRoutes None = []

authMethodRoute :: AuthConfig -> Route
authMethodRoute authConfig = ("authMethod", authMethodHandler authConfig)

authMethodHandler :: AuthConfig -> Snap ()
authMethodHandler authConfig = ok200Json $ m [("authMethod", authMethod authConfig)]
