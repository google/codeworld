{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Util.EmbedAsUrl
  ( embedAsUrl,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text.Encoding as T
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

embedAsUrl :: String -> FilePath -> Q Exp
embedAsUrl contentType f = do
  qAddDependentFile f
  payload <- runIO $ B64.encode <$> B.readFile f
  let uri = "data:" <> BC.pack contentType <> ";base64," <> payload
  [e|
    T.decodeUtf8 $ unsafePerformIO $
      B.unsafePackAddressLen
        $(return $ LitE $ IntegerL $ fromIntegral $ B.length uri)
        $(return $ LitE $ StringPrimL $ B.unpack uri)
    |]
