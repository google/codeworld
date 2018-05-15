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
Snap helper functions
-}

module CodeWorld.Auth.Util
    ( getRequiredParam
    , hoistEither
    , hoistMaybe
    , m
    , withSnapExcept
    ) where

import           CodeWorld.Auth.Http
import           CodeWorld.Auth.Types
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import           Data.ByteString (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import           Snap.Core (Snap, finishWith, getParam)

getRequiredParam :: ByteString -> Snap ByteString
getRequiredParam name = do
    mbValue <- getParam name
    case mbValue of
        Nothing -> finishWith badRequest400
        Just value -> return value

m :: [(String, String)] -> HashMap String String
m = HashMap.fromList

hoistMaybe :: Monad m => e -> Maybe a -> ExceptT e m a
hoistMaybe e mb = maybe (throwE e) return mb

hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither = ExceptT . return

withSnapExcept :: SnapExcept a
withSnapExcept action = runExceptT action >>= either id return
