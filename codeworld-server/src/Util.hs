{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2016 The CodeWorld Authors. All rights reserved.

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
import qualified Crypto.Hash.MD5 as MD5
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.Directory
import           System.IO.Error
import           System.FilePath

newtype BuildMode = BuildMode String deriving Eq

autocompletePath :: FilePath
autocompletePath = "web/codeworld-base.txt"

clientIdPath :: FilePath
clientIdPath = "web/clientId.txt"

buildRootDir :: BuildMode -> FilePath
buildRootDir (BuildMode m) = "data" </> m </> "user"

projectRootDir :: BuildMode -> FilePath
projectRootDir (BuildMode m) = "data" </> m </> "projects"

sourceBase :: Text -> FilePath
sourceBase programId = let s = T.unpack programId in take 3 s </> s

sourceFile :: Text -> FilePath
sourceFile programId = sourceBase programId <.> "hs"

sourceXML :: Text -> FilePath
sourceXML programId = sourceBase programId <.> "xml"

targetFile :: Text -> FilePath
targetFile programId = sourceBase programId <.> "jsexe" </> "all.js"

resultFile :: Text -> FilePath
resultFile programId = sourceBase programId <.> "err.txt"

sourceToProgramId :: ByteString -> Text
sourceToProgramId = ("P" <>) . T.decodeUtf8 . getHash

nameToProjectId :: Text -> Text
nameToProjectId = ("S" <>) . T.decodeUtf8 . getHash . T.encodeUtf8

ensureProgramDir :: BuildMode -> Text -> IO ()
ensureProgramDir mode programId = createDirectoryIfMissing True dir
  where dir = buildRootDir mode </> take 3 (T.unpack programId)

ensureUserProjectDir :: BuildMode -> Text -> IO ()
ensureUserProjectDir mode userId = createDirectoryIfMissing True dir
  where dir = projectRootDir mode </> T.unpack userId

getHash :: ByteString -> ByteString
getHash = BC.takeWhile (/= '=') . BC.map toWebSafe . B64.encode . MD5.hash
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
