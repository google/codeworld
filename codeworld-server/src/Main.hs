{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

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

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString as B
import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import           HIndent (reformat)
import           HIndent.Types (defaultConfig)
import           Network.HTTP.Conduit
import           Snap.Core
import           Snap.Http.Server (quickHttpServe)
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           System.Directory
import           System.FilePath

import Build
import Model
import Util

newtype ClientId = ClientId (Maybe T.Text) deriving (Eq)

main :: IO ()
main = do
    hasClientId <- doesFileExist "web/clientId.txt"
    when (not hasClientId) $ do
        putStrLn "WARNING: Missing web/clientId.txt"
        putStrLn "User logins will not function properly!"

    clientId <- case hasClientId of
        True -> do
            txt <- T.readFile "web/clientId.txt"
            return (ClientId (Just (T.strip txt)))
        False -> return (ClientId Nothing)

    quickHttpServe $ (processBody >> site clientId) <|> site clientId

-- Retrieves the user for the current request.  The request should have an
-- id_token parameter with an id token retrieved from the Google
-- authentication API.  The user is returned if the id token is valid.
getUser :: ClientId -> Snap User
getUser clientId = getParam "id_token" >>= \ case
    Nothing       -> pass
    Just id_token -> do
        let url = "https://www.googleapis.com/oauth2/v1/tokeninfo?id_token=" ++ BC.unpack id_token
        decoded <- fmap decode $ liftIO $ simpleHttp url
        case decoded of
            Nothing -> pass
            Just user -> do
                when (clientId /= ClientId (Just (audience user))) pass
                return user

-- A revised upload policy that allows up to 4 MB of uploaded data in a
-- request.  This is needed to handle uploads of projects including editor
-- history.
codeworldUploadPolicy :: UploadPolicy
codeworldUploadPolicy = setMaximumFormInputSize (2^(22 :: Int)) defaultUploadPolicy

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
getBuildMode = getParam "mode" >>= \ case
    Just "haskell" -> return (BuildMode "haskell")
    Just "blocklyXML" -> return (BuildMode "blocklyXML")
    _              -> return (BuildMode "codeworld")

site :: ClientId -> Snap ()
site clientId =
    route [
      ("loadProject",   loadProjectHandler clientId),
      ("saveProject",   saveProjectHandler clientId),
      ("deleteProject", deleteProjectHandler clientId),
      ("listFolder",    listFolderHandler clientId),
      ("createFolder",  createFolderHandler clientId),
      ("deleteFolder",  deleteFolderHandler clientId),
      ("compile",       compileHandler),
      ("saveXMLhash",   saveXMLHashHandler),
      ("loadXML",       loadXMLHandler),
      ("loadSource",    loadSourceHandler),
      ("run",           runHandler),
      ("runJS",         runHandler),
      ("runMsg",        runMessageHandler),
      ("haskell",       serveFile "web/env.html"),
      ("blocks",        serveFile "web/blocks.html"),
      ("funblocks",     serveFile "web/blocks.html"),
      ("indent",        indentHandler)
    ] <|>
    serveDirectory "web"

-- A DirectoryConfig that sets the cache-control header to avoid errors when new
-- changes are made to JavaScript.
dirConfig :: DirectoryConfig Snap
dirConfig = defaultDirectoryConfig { preServeHook = disableCache }
  where disableCache _ = modifyRequest (addHeader "Cache-control" "no-cache")

createFolderHandler :: ClientId -> Snap ()
createFolderHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    liftIO $ ensureUserProjectDir mode (userId user)
    Just path <- fmap (fmap $ splitDirectories . BC.unpack) $ getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ createDirectory $ userProjectDir mode (userId user) </> finalDir
    liftIO $ B.writeFile (finalDir </> "dir.info") $ BC.pack $ last path

deleteFolderHandler :: ClientId -> Snap ()
deleteFolderHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just path <- fmap (fmap $ splitDirectories . BC.unpack) $ getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureUserDir mode (userId user) finalDir
    liftIO $ removeDirectoryIfExists finalDir

loadProjectHandler :: ClientId -> Snap ()
loadProjectHandler clientId = do
    mode      <- getBuildMode
    user      <- getUser clientId
    Just name <- getParam "name"
    let projectName = T.decodeUtf8 name
    let projectId = nameToProjectId projectName
    Just path <- fmap (fmap $ splitDirectories . BC.unpack) $ getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureProjectDir mode (userId user) finalDir projectId
    let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
    serveFile file

saveProjectHandler :: ClientId -> Snap ()
saveProjectHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just path <- fmap (fmap $ splitDirectories . BC.unpack) $ getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    Just project <- decode . LB.fromStrict . fromJust <$> getParam "project"
    let projectId = nameToProjectId (projectName project)
    liftIO $ ensureProjectDir mode (userId user) finalDir projectId
    let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
    liftIO $ LB.writeFile file $ encode project

deleteProjectHandler :: ClientId -> Snap ()
deleteProjectHandler clientId = do
    mode      <- getBuildMode
    user      <- getUser clientId
    Just name <- getParam "name"
    let projectName = T.decodeUtf8 name
    let projectId = nameToProjectId projectName
    Just path <- fmap (fmap $ splitDirectories . BC.unpack) $ getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureProjectDir mode (userId user) finalDir projectId
    let file = userProjectDir mode (userId user) </> finalDir </> projectFile projectId
    liftIO $ removeFileIfExists file
{-
listProjectsHandler :: ClientId -> Snap ()
listProjectsHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    liftIO $ ensureUserProjectDir mode (userId user)
    let projectDir = userProjectDir mode (userId user)
    hashedDirs <- liftIO $ getDirectoryContentsWithPrefix projectDir
    projectFiles <- liftIO $ fmap concat $ forM hashedDirs getDirectoryContentsWithPrefix
    projects <- liftIO $ fmap catMaybes $ forM projectFiles $ \f -> do
        exists <- doesFileExist f
        if exists then decode <$> LB.readFile f else return Nothing
    modifyResponse $ setContentType "application/json"
    writeLBS (encode (map projectName projects))
-}
{-
getDirectoryContentsWithPrefix :: FilePath -> IO [FilePath]
getDirectoryContentsWithPrefix filePath = do
    withoutPrefix <- getDirectoryContents filePath
    let withPrefix = map (\x -> filePath </> x) $ filter (\x -> not $ x `elem` [".", ".."]) withoutPrefix
    return withPrefix
-}

listFolderHandler :: ClientId -> Snap ()
listFolderHandler clientId = do
    mode <- getBuildMode
    user <- getUser clientId
    Just path <- fmap (fmap $ splitDirectories . BC.unpack) $ getParam "path"
    let dirIds = map (nameToDirId . T.pack) path
    let finalDir = joinPath $ map dirBase dirIds
    liftIO $ ensureUserDir mode (userId user) finalDir
    let projectDir = userProjectDir mode (userId user)
    subHashedDirs <- liftIO $ listDirectoryWithPrefix $ projectDir </> finalDir
    files <- liftIO $ projectFileNames subHashedDirs
    dirs <- liftIO $ projectDirNames subHashedDirs 
    writeLBS (encode (Directory files dirs))

saveXMLHashHandler :: Snap ()
saveXMLHashHandler = do
    mode <- getBuildMode
    unless (mode==BuildMode "blocklyXML") $ modifyResponse $ setResponseCode 500
    Just source <- getParam "source"
    let programId = sourceToProgramId source
    liftIO $ ensureProgramDir mode programId
    liftIO $ B.writeFile (buildRootDir mode </> sourceXML programId) source
    modifyResponse $ setContentType "text/plain"
    writeBS (T.encodeUtf8 (unProgramId programId))

compileHandler :: Snap ()
compileHandler = do
    mode <- getBuildMode
    Just source <- getParam "source"
    let programId = sourceToProgramId source
        deployId = sourceToDeployId source
    success <- liftIO $ do
        ensureProgramDir mode programId
        B.writeFile (buildRootDir mode </> sourceFile programId) source
        writeDeployLink mode deployId programId
        compileIfNeeded mode programId
    when (not success) $ modifyResponse $ setResponseCode 500
    modifyResponse $ setContentType "text/plain"
    let result = CompileResult (unProgramId programId) (unDeployId deployId)
    writeLBS (encode result)

getHashParam :: Bool -> BuildMode -> Snap ProgramId
getHashParam allowDeploy mode = do
    programId <- getParam "hash" >>= \case
      Just h -> return (ProgramId (T.decodeUtf8 h))
      Nothing | allowDeploy -> do
        Just dh <- getParam "dhash"
        let deployId = DeployId (T.decodeUtf8 dh)
        liftIO $ resolveDeployId mode deployId
    return programId

loadXMLHandler :: Snap ()
loadXMLHandler = do
    mode <- getBuildMode
    unless (mode==BuildMode "blocklyXML") $ modifyResponse $ setResponseCode 500
    programId <- getHashParam False mode
    modifyResponse $ setContentType "text/plain"
    serveFile (buildRootDir mode </> sourceXML programId)

loadSourceHandler :: Snap ()
loadSourceHandler = do
    mode <- getBuildMode
    programId <- getHashParam False mode
    modifyResponse $ setContentType "text/x-haskell"
    serveFile (buildRootDir mode </> sourceFile programId)

runHandler :: Snap ()
runHandler = do
    mode <- getBuildMode
    programId <- getHashParam True mode
    liftIO $ compileIfNeeded mode programId
    modifyResponse $ setContentType "text/javascript"
    serveFile (buildRootDir mode </> targetFile programId)

runMessageHandler :: Snap ()
runMessageHandler = do
    mode <- getBuildMode
    programId <- getHashParam False mode
    modifyResponse $ setContentType "text/plain"
    serveFile (buildRootDir mode </> resultFile programId)

indentHandler :: Snap ()
indentHandler = do
    mode <- getBuildMode
    Just source <- getParam "source"
    case reformat defaultConfig Nothing source of
      Left err -> do
        modifyResponse $ setResponseCode 500 . setContentType "text/plain"
        writeLBS $ LB.fromStrict $ BC.pack err
      Right res -> do
        modifyResponse $ setContentType "text/x-haskell"
        writeLBS $ toLazyByteString res
