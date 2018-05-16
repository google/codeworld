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

module CodeWorld.Auth.Secret
    ( Secret(..)
    , generateSecret
    , readSecret
    , writeSecret
    ) where

import           Crypto.Random (getRandomBytes)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (readFile, writeFile)
import           Data.ByteString.Base64 as Base64 (decode, encode)

data Secret = Secret ByteString deriving Eq

generateSecret :: IO Secret
generateSecret = Secret <$> getRandomBytes 32

writeSecret :: FilePath -> Secret -> IO ()
writeSecret path (Secret bytes) = ByteString.writeFile path (Base64.encode bytes)

readSecret :: FilePath -> IO Secret
readSecret path = do
    s <- ByteString.readFile path
    case Base64.decode s of
        Left e -> error e
        Right bytes -> return $ Secret bytes
