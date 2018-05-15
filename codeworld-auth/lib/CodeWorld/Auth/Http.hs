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

module CodeWorld.Auth.Http
    ( badRequest400
    , unauthorized401
    , unauthorized401Json
    , forbidden403
    , json
    , ok200
    , ok200Json
    ) where

import           Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson (encode)
import           Snap.Core
                    ( Response
                    , Snap
                    , emptyResponse
                    , modifyResponse
                    , setContentType
                    , setResponseCode
                    , writeLBS
                    )

emptyResponse' :: Int -> Response
emptyResponse' code = setResponseCode code emptyResponse

ok200 :: Response
ok200 = emptyResponse' 200

unauthorized401 :: Response
unauthorized401 = emptyResponse' 401

forbidden403 :: Response
forbidden403 = emptyResponse' 403

badRequest400 :: Response
badRequest400 = emptyResponse' 400

json :: ToJSON a => Int -> a -> Snap ()
json code obj = do
    modifyResponse $ setResponseCode code . setContentType "application/json"
    writeLBS $ Aeson.encode obj

ok200Json :: ToJSON a => a -> Snap ()
ok200Json = json 200

unauthorized401Json :: ToJSON a => a -> Snap ()
unauthorized401Json = json 401
