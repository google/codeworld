{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC
    -fno-warn-incomplete-patterns
    -fno-warn-name-shadowing
    -fno-warn-unused-imports
    -fno-warn-unused-matches
#-}

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
module Main where

import CodeWorld.Auth
        ( AuthConfig
        , authMethod
        , authenticated
        , authRoutes
        , getAuthConfig
        , optionallyAuthenticated
        )
import Compile
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import Data.Char (isSpace)
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import HIndent (reformat)
import HIndent.Types (defaultConfig)
import Network.HTTP.Conduit
import Snap.Core
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe
import Snap.Util.FileUploads
import System.Directory
import System.FileLock
import System.FilePath

import Model
import Util

-- |A CodeWorld Snap API action
type CodeWorldHandler = AuthConfig -> Snap ()

main :: IO ()
main = do
    appDir <- getCurrentDirectory
    authConfig <- getAuthConfig appDir
    putStrLn $ "Authentication method: " ++ authMethod authConfig
    quickHttpServe $ (processBody >> site authConfig) <|> site authConfig

-- |A public handler that can be called from both authenticated and
-- unauthenticated clients and that does not need access to the optional user
-- ID. If the action needs access to the user ID for authenticated clients,
-- use @optionallyAuthenticated@ directly instead. For handlers that require an
-- authenticated client, and that should reject unauthenticated clients, use
-- @authenticated@.
public :: Snap () -> CodeWorldHandler
public = const

-- A revised upload policy that allows up to 8 MB of uploaded data in a
-- request.  This is needed to handle uploads of projects including editor
-- history.
codeworldUploadPolicy :: UploadPolicy
codeworldUploadPolicy =
    setMaximumFormInputSize (2 ^ (23 :: Int)) defaultUploadPolicy

-- Processes the body of a multipart request.
#if MIN_VERSION_snap_core(1,0,0)
processBody :: Snap ()
processBody = do
    handleMultipart codeworldUploadPolicy (\x y -> return ())
    return ()
#else
processBody :: Snap ()
processBody = do
    handleMultipart codeworldUploadPolicy (\x -> return ())
    return ()
#endif

getBuildMode :: Snap BuildMode
getBuildMode =
    getParam "mode" >>= \case
        Just "haskell" -> return (BuildMode "haskell")
        Just "blocklyXML" -> return (BuildMode "blocklyXML")
        _ -> return (BuildMode "codeworld")

site :: CodeWorldHandler
site authConfig =
    let routes =
            [ ("loadProject", loadProjectHandler authConfig)
            , ("saveProject", saveProjectHandler authConfig)
            , ("deleteProject", deleteProjectHandler authConfig)
            , ("listFolder", listFolderHandler authConfig)
            , ("createFolder", createFolderHandler authConfig)
            , ("deleteFolder", deleteFolderHandler authConfig)
            , ("shareFolder", shareFolderHandler authConfig)
            , ("shareContent", shareContentHandler authConfig)
            , ("moveProject", moveProjectHandler authConfig)
            , ("compile", compileHandler authConfig)
            , ("saveXMLhash", saveXMLHashHandler authConfig)
            , ("loadXML", loadXMLHandler authConfig)
            , ("loadSource", loadSourceHandler authConfig)
            , ("run", runHandler authConfig)
            , ("runJS", runHandler authConfig)
            , ("runMsg", runMessageHandler authConfig)
            , ("haskell", serveFile "web/env.html")
            , ("blocks", serveFile "web/blocks.html")
            , ("funblocks", serveFile "web/blocks.html")
            , ("indent", indentHandler authConfig)
            ]
            ++ authRoutes authConfig
    in route routes <|> serveDirectory "web"

-- A DirectoryConfig that sets the cache-control header to avoid errors when new
-- changes are made to JavaScript.
dirConfig :: DirectoryConfig Snap
dirConfig = defaultDirectoryConfig {preServeHook = disableCache}
  where
    disableCache _ = modifyRequest (addHeader "Cache-control" "no-cache")

createFolderHandler :: CodeWorldHandler
createFolderHandler = authenticated $ \userId -> do
    mode <- getBuildMode
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureUserBaseDir mode userId finalDir
    liftIO $ createDirectory $ userProjectDir mode userId </> finalDir
    modifyResponse $ setContentType "text/plain"
    liftIO $
        B.writeFile
            (userProjectDir mode userId </> finalDir </> "dir.info") $
        BC.pack $ last path

deleteFolderHandler :: CodeWorldHandler
deleteFolderHandler = authenticated $ \userId -> do
    mode <- getBuildMode
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureUserDir mode userId finalDir
    let dir = userProjectDir mode userId </> finalDir
    empty <-
        liftIO $
        fmap
            (\l1 ->
                 length l1 == 3 && sort l1 == sort [".", "..", takeFileName dir])
            (getDirectoryContents (takeDirectory dir))
    liftIO $
        removeDirectoryIfExists $
        if empty
            then takeDirectory dir
            else dir

loadProjectHandler :: CodeWorldHandler
loadProjectHandler = authenticated $ \userId -> do
    mode <- getBuildMode
    Just name <- getParam "name"
    let projectName = T.decodeUtf8 name
    let projectId = nameToProjectId projectName
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureProjectDir mode userId finalDir projectId
    let file =
            userProjectDir mode userId </> finalDir </>
            projectFile projectId
    modifyResponse $ setContentType "application/json"
    serveFile file

saveProjectHandler :: CodeWorldHandler
saveProjectHandler = authenticated $ \userId -> do
    mode <- getBuildMode
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    Just project <- decode . LB.fromStrict . fromJust <$> getParam "project"
    let projectId = nameToProjectId (projectName project)
    liftIO $ ensureProjectDir mode userId finalDir projectId
    let file =
            userProjectDir mode userId </> finalDir </>
            projectFile projectId
    liftIO $ LB.writeFile file $ encode project

deleteProjectHandler :: CodeWorldHandler
deleteProjectHandler = authenticated $ \userId -> do
    mode <- getBuildMode
    Just name <- getParam "name"
    let projectName = T.decodeUtf8 name
    let projectId = nameToProjectId projectName
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureProjectDir mode userId finalDir projectId
    let file =
            userProjectDir mode userId </> finalDir </>
            projectFile projectId
    empty <-
        liftIO $
        fmap
            (\l1 ->
                 length l1 == 3 &&
                 sort l1 == sort [".", "..", takeFileName file])
            (getDirectoryContents (dropFileName file))
    liftIO $
        if empty
            then removeDirectoryIfExists (dropFileName file)
            else removeFileIfExists file

listFolderHandler :: CodeWorldHandler
listFolderHandler = authenticated $ \userId -> do
    mode <- getBuildMode
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureUserBaseDir mode userId finalDir
    liftIO $ ensureUserDir mode userId finalDir
    liftIO $ migrateUser $ userProjectDir mode userId
    let projectDir = userProjectDir mode userId
    subHashedDirs <- liftIO $ listDirectoryWithPrefix $ projectDir </> finalDir
    files <- liftIO $ projectFileNames subHashedDirs
    dirs <- liftIO $ projectDirNames subHashedDirs
    modifyResponse $ setContentType "application/json"
    writeLBS (encode (Directory files dirs))

shareFolderHandler :: CodeWorldHandler
shareFolderHandler = authenticated $ \userId -> do
    mode <- getBuildMode
    Just path <- fmap (splitDirectories . BC.unpack) <$> getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    checkSum <-
        liftIO $ dirToCheckSum $ userProjectDir mode userId </> finalDir
    liftIO $ ensureShareDir mode $ ShareId checkSum
    liftIO $
        B.writeFile (shareRootDir mode </> shareLink (ShareId checkSum)) $
        BC.pack (userProjectDir mode userId </> finalDir)
    modifyResponse $ setContentType "text/plain"
    writeBS $ T.encodeUtf8 checkSum

shareContentHandler :: CodeWorldHandler
shareContentHandler = authenticated $ \userId -> do
    mode <- getBuildMode
    Just shash <- getParam "shash"
    sharingFolder <-
        liftIO $
        B.readFile
            (shareRootDir mode </> shareLink (ShareId $ T.decodeUtf8 shash))
    Just name <- getParam "name"
    let dirPath = dirBase $ nameToDirId $ T.decodeUtf8 name
    liftIO $ ensureUserBaseDir mode userId dirPath
    liftIO $
        copyDirIfExists (BC.unpack sharingFolder) $
        userProjectDir mode userId </> dirPath
    liftIO $
        B.writeFile
            (userProjectDir mode userId </> dirPath </> "dir.info")
            name

moveProjectHandler :: CodeWorldHandler
moveProjectHandler = authenticated $ \userId -> do
    mode <- getBuildMode
    Just moveTo <- fmap (splitDirectories . BC.unpack) <$> getParam "moveTo"
    let moveToDir = joinPath $ map (dirBase . nameToDirId . T.pack) moveTo
    Just moveFrom <- fmap (splitDirectories . BC.unpack) <$> getParam "moveFrom"
    let projectDir = userProjectDir mode userId
    let moveFromDir =
            projectDir </>
            joinPath (map (dirBase . nameToDirId . T.pack) moveFrom)
    let parentFrom =
            if moveFrom == []
                then []
                else init moveFrom
    Just isFile <- getParam "isFile"
    case (moveTo == moveFrom, moveTo == parentFrom, isFile) of
        (False, _, "true") -> do
            Just name <- getParam "name"
            let projectId = nameToProjectId $ T.decodeUtf8 name
                file = moveFromDir </> projectFile projectId
                toFile = projectDir </> moveToDir </> projectFile projectId
            liftIO $ ensureProjectDir mode userId moveToDir projectId
            liftIO $ copyFile file toFile
            empty <-
                liftIO $
                fmap
                    (\l1 ->
                         length l1 == 3 &&
                         sort l1 ==
                         sort [".", "..", takeFileName $ projectFile projectId])
                    (getDirectoryContents
                         (dropFileName $ moveFromDir </> projectFile projectId))
            liftIO $
                if empty
                    then removeDirectoryIfExists
                             (dropFileName $
                              moveFromDir </> projectFile projectId)
                    else removeFileIfExists $
                         moveFromDir </> projectFile projectId
        (_, False, "false") -> do
            let dirName = last $ splitDirectories moveFromDir
            let dir = moveToDir </> take 3 dirName </> dirName
            liftIO $ ensureUserBaseDir mode userId dir
            liftIO $ copyDirIfExists moveFromDir $ projectDir </> dir
            empty <-
                liftIO $
                fmap
                    (\l1 ->
                         length l1 == 3 &&
                         sort l1 == sort [".", "..", takeFileName moveFromDir])
                    (getDirectoryContents (takeDirectory moveFromDir))
            liftIO $
                removeDirectoryIfExists $
                if empty
                    then takeDirectory moveFromDir
                    else moveFromDir
        (_, _, _) -> return ()

withProgramLock :: BuildMode -> ProgramId -> IO a -> IO a
withProgramLock (BuildMode mode) (ProgramId hash) action = do
    tmpDir <- getTemporaryDirectory
    let tmpFile = tmpDir </> "codeworld" <.> T.unpack hash <.> mode
    withFileLock tmpFile Exclusive (const action)

saveXMLHashHandler :: CodeWorldHandler
saveXMLHashHandler = public $ do
    mode <- getBuildMode
    unless (mode == BuildMode "blocklyXML") $
        modifyResponse $ setResponseCode 500
    Just source <- getParam "source"
    let programId = sourceToProgramId source
    liftIO $ withProgramLock mode programId $ do
        ensureSourceDir mode programId
        B.writeFile (sourceRootDir mode </> sourceXML programId) source
    modifyResponse $ setContentType "text/plain"
    writeBS (T.encodeUtf8 (unProgramId programId))

compileHandler :: CodeWorldHandler
compileHandler = public $ do
    mode <- getBuildMode
    Just source <- getParam "source"
    let programId = sourceToProgramId source
        deployId = sourceToDeployId source
    success <- liftIO $ withProgramLock mode programId $ do
        ensureSourceDir mode programId
        B.writeFile (sourceRootDir mode </> sourceFile programId) source
        writeDeployLink mode deployId programId
        compileIfNeeded mode programId
    unless success $ modifyResponse $ setResponseCode 500
    modifyResponse $ setContentType "text/plain"
    let result = CompileResult (unProgramId programId) (unDeployId deployId)
    writeLBS (encode result)

getHashParam :: Bool -> BuildMode -> Snap ProgramId
getHashParam allowDeploy mode =
    getParam "hash" >>= \case
        Just h -> return (ProgramId (T.decodeUtf8 h))
        Nothing
            | allowDeploy -> do
                Just dh <- getParam "dhash"
                let deployId = DeployId (T.decodeUtf8 dh)
                liftIO $ resolveDeployId mode deployId

loadXMLHandler :: CodeWorldHandler
loadXMLHandler = public $ do
    mode <- getBuildMode
    unless (mode == BuildMode "blocklyXML") $
        modifyResponse $ setResponseCode 500
    programId <- getHashParam False mode
    modifyResponse $ setContentType "text/plain"
    serveFile (sourceRootDir mode </> sourceXML programId)

loadSourceHandler :: CodeWorldHandler
loadSourceHandler = public $ do
    mode <- getBuildMode
    programId <- getHashParam False mode
    modifyResponse $ setContentType "text/x-haskell"
    serveFile (sourceRootDir mode </> sourceFile programId)

runHandler :: CodeWorldHandler
runHandler = public $ do
    mode <- getBuildMode
    programId <- getHashParam True mode
    liftIO $ compileIfNeeded mode programId
    modifyResponse $ setContentType "text/javascript"
    serveFile (buildRootDir mode </> targetFile programId)

runMessageHandler :: CodeWorldHandler
runMessageHandler = public $ do
    mode <- getBuildMode
    programId <- getHashParam False mode
    modifyResponse $ setContentType "text/plain"
    serveFile (buildRootDir mode </> resultFile programId)

indentHandler :: CodeWorldHandler
indentHandler = public $ do
    mode <- getBuildMode
    Just source <- getParam "source"
    case reformat defaultConfig Nothing Nothing source of
        Left err -> do
            modifyResponse $ setResponseCode 500 . setContentType "text/plain"
            writeLBS $ LB.fromStrict $ BC.pack err
        Right res -> do
            modifyResponse $ setContentType "text/x-haskell"
            writeLBS $ toLazyByteString res

compileIfNeeded :: BuildMode -> ProgramId -> IO Bool
compileIfNeeded mode programId = do
    hasResult <- doesFileExist (buildRootDir mode </> resultFile programId)
    hasTarget <- doesFileExist (buildRootDir mode </> targetFile programId)
    if hasResult
        then return hasTarget
        else compileSource
                 FullBuild
                 (sourceRootDir mode </> sourceFile programId)
                 (buildRootDir mode </> targetFile programId)
                 (buildRootDir mode </> resultFile programId)
                 (getMode mode)

getMode :: BuildMode -> String
getMode (BuildMode m) = m
