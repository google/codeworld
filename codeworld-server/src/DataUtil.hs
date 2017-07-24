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

module DataUtil where

import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash as Crypto
import           Data.Aeson
import           Data.ByteArray (convert)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LB
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.Directory
import           System.IO.Error
import           System.FilePath
import           System.File.Tree (getDirectory, copyTo_)
import           System.Posix.Files

import Model

newtype BuildMode = BuildMode String deriving Eq
newtype ProgramId = ProgramId { unProgramId :: Text } deriving Eq
newtype ProjectId = ProjectId { unProjectId :: Text } deriving Eq
newtype DeployId  = DeployId  { unDeployId  :: Text } deriving Eq
newtype DirId     = DirId     { unDirId     :: Text}  deriving Eq
newtype ShareId   = ShareId   { unShareId   :: Text } deriving Eq

autocompletePath :: FilePath
autocompletePath = "web/codeworld-base.txt"

clientIdPath :: FilePath
clientIdPath = "web/clientId.txt"

buildRootDir :: BuildMode -> FilePath
buildRootDir (BuildMode m) = "data" </> m </> "user"

shareRootDir :: BuildMode -> FilePath
shareRootDir (BuildMode m) = "data" </> m </> "share"

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

shareLink :: ShareId -> FilePath
shareLink (ShareId sh) = let s = T.unpack sh in take 3 s </> s

userProjectDir :: BuildMode -> Text -> FilePath
userProjectDir mode userId' = projectRootDir mode </> T.unpack userId'

projectBase :: ProjectId -> FilePath
projectBase (ProjectId p) = let s = T.unpack p in take 3 s </> s

projectFile :: ProjectId -> FilePath
projectFile projectId = projectBase projectId <.> "cw"

sourceToProgramId :: ByteString -> ProgramId
sourceToProgramId = ProgramId . hashToId "P"

sourceToDeployId :: ByteString -> DeployId
sourceToDeployId = DeployId . hashToId "D" . ("DEPLOY_ID" <>)

nameToProjectId :: Text -> ProjectId
nameToProjectId = ProjectId . hashToId "S" . T.encodeUtf8

dirBase :: DirId -> FilePath
dirBase (DirId d) = let s = T.unpack d in take 3 s </> s

nameToDirId :: Text -> DirId
nameToDirId = DirId . hashToId "D" . T.encodeUtf8

ensureProgramDir :: BuildMode -> ProgramId -> IO ()
ensureProgramDir mode (ProgramId p) = createDirectoryIfMissing True dir
  where dir = buildRootDir mode </> take 3 (T.unpack p)

ensureShareDir :: BuildMode -> ShareId -> IO ()
ensureShareDir mode (ShareId s) = createDirectoryIfMissing True dir
  where dir = shareRootDir mode </> take 3 (T.unpack s)

ensureUserProjectDir :: BuildMode -> Text -> IO ()
ensureUserProjectDir mode userId' =
    createDirectoryIfMissing True (userProjectDir mode userId')

ensureUserBaseDir :: BuildMode -> Text -> FilePath -> IO ()
ensureUserBaseDir mode userId' path = do
    ensureUserProjectDir mode userId'
    createDirectoryIfMissing False (userProjectDir mode userId' </> takeDirectory path)

ensureUserDir :: BuildMode -> Text -> FilePath -> IO ()
ensureUserDir mode userId' path = do
    ensureUserProjectDir mode userId'
    createDirectoryIfMissing False (userProjectDir mode userId' </> path)

ensureProjectDir :: BuildMode -> Text -> FilePath -> ProjectId -> IO ()
ensureProjectDir mode userId' path projectId = do
    ensureUserProjectDir mode userId'
    createDirectoryIfMissing False (dropFileName f)
  where f = userProjectDir mode userId' </> path </> projectFile projectId

createNewFolder :: BuildMode -> Text -> FilePath -> FilePath -> IO ()
createNewFolder mode userId' finalDir name = do
    ensureUserBaseDir mode userId' finalDir
    ensureUserDir mode userId' finalDir
    B.writeFile (userProjectDir mode userId' </> finalDir </> "dir.info") $ BC.pack name

listDirectoryWithPrefix :: FilePath -> IO [FilePath]
listDirectoryWithPrefix filePath = map (filePath </>) <$> listDirectory filePath

dirFilter :: [FilePath] -> Char -> IO [FilePath]
dirFilter dirs' char = fmap concat $ mapM listDirectoryWithPrefix $
    filter (\x -> head (takeBaseName x) == char) dirs'

projectFileNames :: [FilePath] -> IO [Text]
projectFileNames subHashedDirs = do
    hashedFiles <- dirFilter subHashedDirs 'S'
    projects <- fmap catMaybes $ forM hashedFiles $ \f -> do
        exists <- doesFileExist f
        case reverse f  of
          x | take 3 x == "wc." && length x == 26 ->
               if exists then (fmap projectName) <$> (decode <$> LB.readFile f)
                         else return Nothing
          x | take 5 x == "ofni." && length x == 28 ->
               if exists then Just . T.decodeUtf8 <$> B.readFile f else return Nothing
          _ -> return Nothing
    return projects

projectDirNames :: [FilePath] -> IO [Text]
projectDirNames subHashedDirs = do
    hashedDirs <- dirFilter subHashedDirs 'D'
    dirs' <- mapM (\x -> B.readFile $ x </> "dir.info") hashedDirs
    return $ map T.decodeUtf8 dirs'

writeDeployLink :: BuildMode -> DeployId -> ProgramId -> IO ()
writeDeployLink mode deployId (ProgramId p) = do
    createDirectoryIfMissing True (dropFileName f)
    B.writeFile f (T.encodeUtf8 p)
  where f = deployRootDir mode </> deployLink deployId

resolveDeployId :: BuildMode -> DeployId -> IO ProgramId
resolveDeployId mode deployId = ProgramId . T.decodeUtf8 <$> B.readFile f
  where f = deployRootDir mode </> deployLink deployId

isDir :: FilePath -> IO Bool
isDir path = do
    status <- getFileStatus path
    return $ isDirectory status

migrateUser :: FilePath -> IO ()
migrateUser userRoot = do
    prevContent <- filter (\x -> (take 3 (reverse x) == "wc.") && (length x == 26)) <$>
      listDirectory userRoot
    mapM_ (\x -> createDirectoryIfMissing False $ userRoot </> take 3 x) prevContent
    mapM_ (\x -> renameFile (userRoot </> x) $ userRoot </> take 3 x </> x) prevContent

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive path = do
    dirBool <- isDir path
    case dirBool of
      True -> do
        case path of
          x | isSuffixOf ".comments" (drop 23 x) -> return []
            | isSuffixOf ".comments.users" (drop 23 x) -> return []
            | isSuffixOf ".comments.versions" (drop 23 x) -> return []
            | otherwise -> do
               contents <- listDirectory path
               concat <$> mapM (getFilesRecursive . (path </>)) contents
      False -> case reverse path of
                 x | isSuffixOf ".info" (drop 23 x) -> return []
                   | x == "dir.info" -> return []
                   | otherwise -> return [path]

dirToCheckSum :: FilePath -> IO Text
dirToCheckSum path = do
    files' <- getFilesRecursive path
    fileContents <- mapM B.readFile files'
    let cryptoContext = Crypto.hashInitWith Crypto.MD5
    return $ (T.pack "F" <>)
           . T.decodeUtf8
           . BC.takeWhile (/= '=')
           . BC.map toWebSafe
           . B64.encode
           . convert
           . Crypto.hashFinalize
           . Crypto.hashUpdates cryptoContext $ fileContents
    where toWebSafe '/' = '_'
          toWebSafe '+' = '-'
          toWebSafe c   = c

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

copyDirIfExists :: FilePath -> FilePath -> IO ()
copyDirIfExists folder1 folder2 = (getDirectory folder1 >>= copyTo_ folder2) `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists dirName = removeDirectoryRecursive dirName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
