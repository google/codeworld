{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC
    -fno-warn-name-shadowing
#-}

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
module Util where

import CodeWorld.Account (UserId(..))
import Control.Exception
import Control.Monad
import qualified Crypto.Hash as Crypto
import Data.Aeson
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.List (sort, sortOn)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.IO.Error
import System.Posix.Files

import Model

newtype BuildMode = BuildMode String
    deriving (Eq)

newtype ProgramId = ProgramId
    { unProgramId :: Text
    } deriving (Eq)

newtype ProjectId = ProjectId
    { unProjectId :: Text
    } deriving (Eq)

newtype DeployId = DeployId
    { unDeployId :: Text
    } deriving (Eq)

newtype DirId = DirId
    { unDirId :: Text
    } deriving (Eq)

newtype ShareId = ShareId
    { unShareId :: Text
    } deriving (Eq)

type BaseVersion = Text

autocompletePath :: FilePath
autocompletePath = "web/codeworld-base.txt"

clientIdPath :: FilePath
clientIdPath = "web/clientId.txt"

baseRootDir :: FilePath
baseRootDir = "data/base"

sourceRootDir :: BuildMode -> FilePath
sourceRootDir (BuildMode m) = "data" </> m </> "user"

buildRootDir :: BuildMode -> FilePath
buildRootDir (BuildMode m) = "data" </> m </> "build"

shareRootDir :: BuildMode -> FilePath
shareRootDir (BuildMode m) = "data" </> m </> "share"

projectRootDir :: BuildMode -> FilePath
projectRootDir (BuildMode m) = "data" </> m </> "projects"

deployRootDir :: BuildMode -> FilePath
deployRootDir (BuildMode m) = "data" </> m </> "deploy"

baseCodeFile :: BaseVersion -> FilePath
baseCodeFile ver = baseRootDir </> T.unpack ver </> "base.js"

baseSymbolFile :: BaseVersion -> FilePath
baseSymbolFile ver = baseRootDir </> T.unpack ver </> "base.symbs"

sourceBase :: ProgramId -> FilePath
sourceBase (ProgramId p) =
    let s = T.unpack p
    in take 3 s </> s

sourceFile :: ProgramId -> FilePath
sourceFile programId = sourceBase programId <.> "hs"

sourceXML :: ProgramId -> FilePath
sourceXML programId = sourceBase programId <.> "xml"

targetFile :: ProgramId -> FilePath
targetFile programId = sourceBase programId <.> "js"

resultFile :: ProgramId -> FilePath
resultFile programId = sourceBase programId <.> "err.txt"

baseVersionFile :: ProgramId -> FilePath
baseVersionFile programId = sourceBase programId <.> "basever"

auxiliaryFiles :: ProgramId -> [FilePath]
auxiliaryFiles programId =
    [ sourceBase programId <.> "js_hi"
    , sourceBase programId <.> "js_o"
    , sourceBase programId <.> "jsexe" </> "index.html"
    , sourceBase programId <.> "jsexe" </> "lib.js"
    , sourceBase programId <.> "jsexe" </> "manifest.webapp"
    , sourceBase programId <.> "jsexe" </> "out.js"
    , sourceBase programId <.> "jsexe" </> "out.stats"
    , sourceBase programId <.> "jsexe" </> "rts.js"
    , sourceBase programId <.> "jsexe" </> "runmain.js"
    ]

deployLink :: DeployId -> FilePath
deployLink (DeployId d) =
    let s = T.unpack d
    in take 3 s </> s

shareLink :: ShareId -> FilePath
shareLink (ShareId sh) =
    let s = T.unpack sh
    in take 3 s </> s

userProjectDir :: BuildMode -> UserId -> FilePath
userProjectDir mode (UserId userIdRaw) = projectRootDir mode </> userIdRaw

projectBase :: ProjectId -> FilePath
projectBase (ProjectId p) = T.unpack p

projectFile :: ProjectId -> FilePath
projectFile projectId = projectBase projectId <.> "cw"

sourceToProgramId :: ByteString -> ProgramId
sourceToProgramId = ProgramId . T.map dashToUnderscore . hashToId "P"

sourceToDeployId :: ByteString -> DeployId
sourceToDeployId = DeployId . T.map dashToUnderscore . hashToId "D" . ("DEPLOY_ID" <>)

dashToUnderscore :: Char -> Char
dashToUnderscore '-' = '_'
dashToUnderscore c = c

nameToProjectId :: Text -> ProjectId
nameToProjectId = ProjectId . hashToId "S" . T.encodeUtf8

dirBase :: DirId -> FilePath
dirBase (DirId d) = T.unpack d

nameToDirId :: Text -> DirId
nameToDirId = DirId . hashToId "D" . T.encodeUtf8

ensureSourceDir :: BuildMode -> ProgramId -> IO ()
ensureSourceDir mode (ProgramId p) = createDirectoryIfMissing True dir
  where
    dir = sourceRootDir mode </> take 3 (T.unpack p)

ensureShareDir :: BuildMode -> ShareId -> IO ()
ensureShareDir mode (ShareId s) = createDirectoryIfMissing True dir
  where
    dir = shareRootDir mode </> take 3 (T.unpack s)

ensureUserProjectDir :: BuildMode -> UserId -> IO ()
ensureUserProjectDir mode userId =
    createDirectoryIfMissing True (userProjectDir mode userId)

ensureUserBaseDir :: BuildMode -> UserId -> FilePath -> IO ()
ensureUserBaseDir mode userId path = do
    ensureUserProjectDir mode userId
    createDirectoryIfMissing
        False
        (userProjectDir mode userId </> takeDirectory path)

ensureUserDir :: BuildMode -> UserId -> FilePath -> IO ()
ensureUserDir mode userId path = do
    ensureUserProjectDir mode userId
    createDirectoryIfMissing False (userProjectDir mode userId </> path)

ensureProjectDir :: BuildMode -> UserId -> FilePath -> ProjectId -> IO ()
ensureProjectDir mode userId path projectId = do
    ensureUserProjectDir mode userId
    createDirectoryIfMissing False (dropFileName f)
  where
    f = userProjectDir mode userId </> path </> projectFile projectId

listDirectoryWithPrefix :: FilePath -> IO [FilePath]
listDirectoryWithPrefix filePath = map (filePath </>) <$> listDirectory filePath

listDirectoryWithPrefixRecursive :: FilePath -> IO [FilePath]
listDirectoryWithPrefixRecursive filePath = do
    subpaths <- map (filePath </>) <$> listDirectory filePath
    dirs <- filterM doesDirectoryExist subpaths
    subtrees <- mapM listDirectoryWithPrefixRecursive dirs
    return $ subpaths ++ (concat subtrees)

dirFilter :: [FilePath] -> Char -> [FilePath]
dirFilter dirs char = filter (\x -> head (takeBaseName x) == char) dirs

fsEntries :: FilePath -> IO [FileSystemEntry]
fsEntries dir = do
    subHashedDirs <- listDirectoryWithPrefix dir
    let hashedFiles = dirFilter subHashedDirs 'S'
        hashedDirs = dirFilter subHashedDirs 'D'
    projNames <- sort <$> mapM projName hashedFiles
    dirNames  <- sort <$> mapM dirName hashedDirs
    haveSavedOrderFile <- doesFileExist $ dir </> "order.info"
    case haveSavedOrderFile of
        True -> do
            dumpedEntries <- fromJust . decode <$> LB.readFile (dir </> "order.info")
            let (dumpedDirs, dumpedProjects) = span (\x -> fsEntryType x == Dir) $ sortOn fsEntryType dumpedEntries
                existingDirs = onlyExisting (sortOn fsEntryName dumpedDirs) dirNames
                existingProjects = onlyExisting (sortOn fsEntryName dumpedProjects)  projNames 
            return $
                    updateOrder Dir existingDirs dirNames ++
                    updateOrder Proj existingProjects projNames
        False -> return $ updateOrder Dir [] dirNames ++ updateOrder Proj [] projNames
    where
        onlyExisting :: [FileSystemEntry] -> [Text] -> [FileSystemEntry]
        onlyExisting dumped existing = filter (\d -> fsEntryName d `elem` existing) dumped

        updateOrder :: FileSystemEntryType -> [FileSystemEntry] -> [Text] -> [FileSystemEntry]
        updateOrder _ [] [] = []
        updateOrder _ (_:_) [] = []
        updateOrder defType [] (name:names) = (FSEntry 0 name defType) : updateOrder defType [] names
        updateOrder defType (entry@(FSEntry _ dumpedName _):entries) (name:names)
            | dumpedName == name = entry : updateOrder defType entries names
            | otherwise = (FSEntry 0 name defType) : updateOrder defType (entry: entries) names

        projName path = do
            Just project <- decode <$> LB.readFile path
            return $ projectName project
        dirName path = T.readFile (path </> "dir.info")

projectFileNames :: FilePath -> IO [Text]
projectFileNames dir = do
    subHashedDirs <- listDirectoryWithPrefix dir
    let hashedFiles = dirFilter subHashedDirs 'S'
    projects <- fmap catMaybes $
        forM hashedFiles $ \f -> do
            exists <- doesFileExist f
            if exists
                then decode <$> LB.readFile f
                else return Nothing
    return $ map projectName projects

projectDirNames :: FilePath -> IO [Text]
projectDirNames dir = do
    subHashedDirs <- listDirectoryWithPrefix dir
    let hashedDirs = dirFilter subHashedDirs 'D'
    dirNames <- mapM (\x -> T.readFile $ x </> "dir.info") hashedDirs
    return dirNames

writeDeployLink :: BuildMode -> DeployId -> ProgramId -> IO ()
writeDeployLink mode deployId (ProgramId p) = do
    createDirectoryIfMissing True (dropFileName f)
    B.writeFile f (T.encodeUtf8 p)
  where
    f = deployRootDir mode </> deployLink deployId

resolveDeployId :: BuildMode -> DeployId -> IO ProgramId
resolveDeployId mode deployId = ProgramId . T.decodeUtf8 <$> B.readFile f
  where
    f = deployRootDir mode </> deployLink deployId

isDir :: FilePath -> IO Bool
isDir path = do
    status <- getFileStatus path
    return $ isDirectory status

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive path = do
    dirBool <- isDir path
    case dirBool of
        True -> do
            contents <- listDirectory path
            concat <$> mapM (getFilesRecursive . (path </>)) contents
        False -> return [path]

dirToCheckSum :: FilePath -> IO Text
dirToCheckSum path = do
    files <- getFilesRecursive path
    fileContents <- mapM B.readFile files
    let cryptoContext = Crypto.hashInitWith Crypto.MD5
    return $
        (T.pack "F" <>) .
        T.takeWhile (/= '=') .
        T.map toWebSafe .
        T.decodeUtf8 .
        B64.encode .
        convert . Crypto.hashFinalize . Crypto.hashUpdates cryptoContext $
        fileContents
  where
    toWebSafe '/' = '_'
    toWebSafe '+' = '-'
    toWebSafe c = c

hashToId :: Text -> ByteString -> Text
hashToId pfx =
    (pfx <>) .
    T.takeWhile (/= '=') .
    T.map toWebSafe .
    T.decodeUtf8 .
    B64.encode .
    convert .
    Crypto.hashWith Crypto.MD5
  where
    toWebSafe '/' = '_'
    toWebSafe '+' = '-'
    toWebSafe c = c

copyDirIfExists :: FilePath -> FilePath -> IO ()
copyDirIfExists src dst = do
    contents <- listDirectory src
    dstExists <- doesDirectoryExist dst
    when (not dstExists) $ createDirectoryIfMissing True dst
    forM_ contents $ \f -> do
        let srcPath = src </> f
        let dstPath = dst </> f
        isDir <- doesDirectoryExist srcPath
        if isDir
          then copyDirIfExists srcPath dstPath
          else copyFile srcPath dstPath

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists dirName =
    removeDirectoryRecursive dirName `catch` handleExists
  where
    handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
