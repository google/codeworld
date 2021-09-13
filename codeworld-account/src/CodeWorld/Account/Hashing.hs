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

module CodeWorld.Account.Hashing
  ( hash,
    validate,
  )
where

import CodeWorld.Account.Types
import Crypto.BCrypt
    ( hashPasswordUsingPolicy
    , slowerBcryptHashingPolicy
    , validatePassword
    )
import qualified Data.ByteString.Char8 as Char8 (pack)

-- | Hashes a password using default bcrypt algorithm
hash ::
  -- | Password
  Password ->
  -- | Password hash
  IO PasswordHash
hash (Password passwordRaw) = do
  mbPasswordHashRaw <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (Char8.pack passwordRaw)
  case mbPasswordHashRaw of
    Nothing              -> error "Assertion failed"
    Just passwordHashRaw -> return $ PasswordHash passwordHashRaw

-- | Validates a password against a bcrypt password hash
validate ::
  -- | Password hash
  PasswordHash ->
  -- | Password
  Password ->
  -- | True if password is valid, False otherwise
  Bool
validate (PasswordHash passwordHashRaw) (Password passwordRaw) =
  validatePassword passwordHashRaw (Char8.pack passwordRaw)
