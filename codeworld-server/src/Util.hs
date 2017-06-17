{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright 2017 The CodeWorld Authors. All rights reserved.

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
import qualified Crypto.Hash as Crypto
import           Data.ByteArray (convert)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
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
newtype ProgramId = ProgramId { unProgramId :: Text } deriving Eq
newtype ProjectId = ProjectId { unProjectId :: Text } deriving Eq
newtype DeployId  = DeployId  { unDeployId  :: Text } deriving Eq

autocompletePath :: FilePath
autocompletePath = "web/codeworld-base.txt"

clientIdPath :: FilePath
clientIdPath = "web/clientId.txt"

buildRootDir :: BuildMode -> FilePath
buildRootDir (BuildMode m) = "data" </> m </> "user"

projectRootDir :: BuildMode -> FilePath
projectRootDir (BuildMode m) = "data" </> m </> "projects"

deployRootDir :: BuildMode -> FilePath
deployRootDir (BuildMode m) = "data" </> m </> "deploy"

sourceBase :: ProgramId -> FilePath
sourceBase (ProgramId p) = let s = T.unpack p in take 3 s </> s

sourceFile :: ProgramId -> FilePath
sourceFile programId = sourceBase programId <.> "hs"

sourceXML :: ProgramId -> FilePath
sourceXML programId = sourceBase programId <.> "xml"

targetFile :: ProgramId -> FilePath
targetFile programId = sourceBase programId <.> "js"

resultFile :: ProgramId -> FilePath
resultFile programId = sourceBase programId <.> "err.txt"

{-
rawResultFile :: ProgramId -> FilePath
rawResultFile programId = sourceBase programId <.> "rawerr.txt"
-}

auxiliaryFiles :: ProgramId -> [FilePath]
auxiliaryFiles programId = [
    sourceBase programId <.> "js_hi",
    sourceBase programId <.> "js_o",
    sourceBase programId <.> "jsexe" </> "index.html",
    sourceBase programId <.> "jsexe" </> "lib.js",
    sourceBase programId <.> "jsexe" </> "manifest.webapp",
    sourceBase programId <.> "jsexe" </> "out.js",
    sourceBase programId <.> "jsexe" </> "out.stats",
    sourceBase programId <.> "jsexe" </> "rts.js",
    sourceBase programId <.> "jsexe" </> "runmain.js"
    ]

deployLink :: DeployId -> FilePath
deployLink (DeployId d) = let s = T.unpack d in take 3 s </> s

userProjectDir :: BuildMode -> Text -> FilePath
userProjectDir mode userId = projectRootDir mode </> T.unpack userId

projectFile :: ProjectId -> FilePath
projectFile (ProjectId p) = let s = T.unpack p in s <.> "cw"

sourceToProgramId :: ByteString -> ProgramId
sourceToProgramId = ProgramId . hashToId "P"

sourceToDeployId :: ByteString -> DeployId
sourceToDeployId = DeployId . hashToId "D" . ("DEPLOY_ID" <>)

nameToProjectId :: Text -> ProjectId
nameToProjectId = ProjectId . hashToId "S" . T.encodeUtf8

ensureProgramDir :: BuildMode -> ProgramId -> IO ()
ensureProgramDir mode (ProgramId p) = createDirectoryIfMissing True dir
  where dir = buildRootDir mode </> take 3 (T.unpack p)

ensureUserProjectDir :: BuildMode -> Text -> IO ()
ensureUserProjectDir mode userId =
    createDirectoryIfMissing True (userProjectDir mode userId)

writeDeployLink :: BuildMode -> DeployId -> ProgramId -> IO ()
writeDeployLink mode deployId (ProgramId p) = do
    createDirectoryIfMissing True (dropFileName f)
    B.writeFile f (T.encodeUtf8 p)
  where f = deployRootDir mode </> deployLink deployId

resolveDeployId :: BuildMode -> DeployId -> IO ProgramId
resolveDeployId mode deployId = ProgramId . T.decodeUtf8 <$> B.readFile f
  where f = deployRootDir mode </> deployLink deployId

hashToId :: Text -> ByteString -> Text
hashToId pfx = (pfx <>)
             . T.decodeUtf8
             . BC.takeWhile (/= '=')
             . BC.map toWebSafe
             . B64.encode
             . convert
             . Crypto.hashWith Crypto.MD5
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
