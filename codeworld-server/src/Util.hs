{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2015 Google Inc. All rights reserved.

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

module Util where

import           Control.Concurrent
import           Control.Exception
import qualified Crypto.Hash.MD5 as Crypto
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import           Data.List
import           System.Directory
import           System.IO.Error

getFilesByExt :: FilePath -> FilePath -> IO [FilePath]
getFilesByExt ext = fmap (sort . filter (ext `isSuffixOf`)) . getDirectoryContents

getHash :: ByteString -> ByteString
getHash = BC.map toWebSafe . B64.encode . Crypto.hash
  where toWebSafe '/' = '_'
        toWebSafe '+' = '-'
        toWebSafe c   = c

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout micros action = do
    result <- newEmptyMVar
    killer <- forkIO $ threadDelay micros >> putMVar result Nothing
    runner <- forkIO $ action >>= putMVar result . Just
    r <- takeMVar result
    killThread killer
    killThread runner
    return r
