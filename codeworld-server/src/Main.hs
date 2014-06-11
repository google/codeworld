{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Crypto.Hash.MD5 as Crypto
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Conduit
import           Snap.Core
import           Snap.Http.Server (quickHttpServe)
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

data User = User {
    userId :: Text
    }

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "user_id"
    parseJSON _          = mzero

data Project = Project {
    projectName :: Text,
    projectSource :: Text,
    projectHistory :: Value
    }

instance FromJSON Project where
    parseJSON (Object v) = Project <$> v .: "name"
                                   <*> v .: "source"
                                   <*> v .: "history"
    parseJSON _          = mzero

instance ToJSON Project where
    toJSON p = object [ "name"    .= projectName p,
                        "source"  .= projectSource p,
                        "history" .= projectHistory p ]

getUser :: Snap User
getUser = getParam "id_token" >>= \ case
    Nothing       -> pass
    Just id_token -> maybe pass return =<< (fmap decode $ liftIO $ simpleHttp $
        "https://www.googleapis.com/oauth2/v1/tokeninfo?id_token=" ++ BC.unpack id_token)

main :: IO ()
main = do
    generateBaseBundle
    quickHttpServe $ (processBody >> site) <|> site

processBody :: Snap ()
processBody = do
    handleMultipart defaultUploadPolicy (const $ return ())
    return ()

site :: Snap ()
site =
    route [
      ("loadProject",   loadProjectHandler),
      ("saveProject",   saveProjectHandler),
      ("deleteProject", deleteProjectHandler),
      ("compile",       compileHandler),
      ("run",           runHandler),
      ("runJS",         runHandler),
      ("listExamples",  listExamplesHandler),
      ("listProjects",  listProjectsHandler)
    ] <|>
    dir "user" (serveDirectory "user") <|>
    serveDirectory "web"

loadProjectHandler :: Snap ()
loadProjectHandler = do
    user      <- getUser
    Just name <- getParam "name"
    let hash = T.decodeUtf8 (getHash name)
    let fname = "projects" </> T.unpack (hash <> "." <> userId user <> ".cw")
    serveFile fname

saveProjectHandler :: Snap ()
saveProjectHandler = do
    user <- getUser
    Just project <- decode . LB.fromStrict . fromJust <$> getParam "project"
    let hash = getHash (T.encodeUtf8 (projectName project))
    let fname = T.decodeUtf8 hash <> "." <> userId user <> ".cw"
    liftIO $ LB.writeFile ("projects" </> T.unpack fname) $ encode project

deleteProjectHandler :: Snap ()
deleteProjectHandler = do
    user      <- getUser
    Just name <- getParam "name"
    let hash = T.decodeUtf8 (getHash name)
    let fname = "projects" </> T.unpack (hash <> "." <> userId user <> ".cw")
    liftIO $ removeFile fname

compileHandler :: Snap ()
compileHandler = do
    Just source <- getParam "source"
    let hashed = BC.cons 'P' (getHash source)
    liftIO $ do
        B.writeFile (sourceFile hashed) source
        compileIfNeeded hashed
    hasTarget <- liftIO $ doesFileExist (targetFile hashed)
    when (not hasTarget) $ modifyResponse $ setResponseCode 500
    writeBS hashed

runHandler :: Snap ()
runHandler = do
    Just hashed <- getParam "hash"
    liftIO $ compileIfNeeded hashed
    serveFile (targetFile hashed)

listExamplesHandler :: Snap ()
listExamplesHandler = do
    files <- liftIO $ getFilesByExt ".hs" "web/examples"
    writeLBS (encode files)

listProjectsHandler :: Snap ()
listProjectsHandler = do
    user  <- getUser
    projects <- liftIO $ do
        let ext = T.unpack $ "." <> userId user <> ".cw"
        let base = "projects"
        files <- getFilesByExt ext base
        mapM (fmap (fromJust . decode) . LB.readFile . (base </>)) files :: IO [Project]
    writeLBS (encode (map projectName projects))

getFilesByExt :: FilePath -> FilePath -> IO [FilePath]
getFilesByExt ext = fmap (sort . filter (ext `isSuffixOf`)) . getDirectoryContents

getHash :: ByteString -> ByteString
getHash = BC.map toWebSafe . B64.encode . Crypto.hash
  where toWebSafe '/' = '_'
        toWebSafe '+' = '-'
        toWebSafe c   = c

compileIfNeeded :: ByteString -> IO ()
compileIfNeeded hashed = do
    hasResult <- doesFileExist (resultFile  hashed)
    needsRebuild <- if not hasResult then return True else do
        rebuildTime <- getModificationTime rebuildFile
        buildTime   <- getModificationTime (resultFile hashed)
        return (buildTime < rebuildTime)
    when needsRebuild $ compileUserSource (localSourceFile hashed) (resultFile hashed)

rebuildFile :: FilePath
rebuildFile = "user" </> "REBUILD"

localSourceFile :: ByteString -> FilePath
localSourceFile hashed = BC.unpack hashed ++ ".hs"

sourceFile :: ByteString -> FilePath
sourceFile hashed = "user" </> localSourceFile hashed

targetFile :: ByteString -> FilePath
targetFile hashed = "user" </> BC.unpack hashed ++ ".jsexe" </> "out.js"

resultFile :: ByteString -> FilePath
resultFile hashed = "user" </> BC.unpack hashed ++ ".err.txt"

commonGHCJSArgs :: [String]
commonGHCJSArgs = [
    "--no-native",
    "-Wall",
    "-O2",
    "-fno-warn-deprecated-flags",
    "-fno-warn-amp",
    "-fno-warn-missing-signatures",
    "-fdefer-type-errors",
    "-hide-package", "base",
    "-package", "codeworld-base",
    "-XRebindableSyntax",
    "-XImplicitPrelude",
    "-XOverloadedStrings",
    "-XNoTemplateHaskell",
    "-XNoUndecidableInstances",
    "-XNoQuasiQuotes",
    "-XExplicitForAll",
    "-XJavaScriptFFI"
    ]

generateBaseBundle :: IO ()
generateBaseBundle = do
    let ghcjsArgs = commonGHCJSArgs ++ [
            "--generate-base=LinkBase",
            "-o", "base",
            "LinkMain.hs"
          ]
    BC.putStrLn =<< runCompiler "ghcjs" ghcjsArgs
    B.appendFile "user/REBUILD" ""
    return ()

compileUserSource :: FilePath -> FilePath -> IO ()
compileUserSource sourcePath resultPath = do
    let ghcjsArgs = commonGHCJSArgs ++ [
            "--no-rts",
            "--no-stats",
            "--use-base=base.jsexe/out.base.symbs",
            "./" ++ sourcePath
          ]
    result <- runCompiler "ghcjs" ghcjsArgs
    B.writeFile resultPath $
        sanitizeError sourcePath result

runCompiler :: FilePath -> [String] -> IO ByteString
runCompiler cmd args = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ cwd       = Just "user",
                                       std_in    = CreatePipe,
                                       std_out   = CreatePipe,
                                       std_err   = CreatePipe,
                                       close_fds = True }

    hClose inh

    err <- B.hGetContents errh
    evaluate (B.length err)

    waitForProcess pid

    hClose outh
    return err

sanitizeError :: FilePath -> ByteString -> ByteString
sanitizeError source = T.encodeUtf8
                     . T.replace (T.pack source) "your program"
                     . T.replace "IO action main" "variable main"
                     . T.replace "in module Main" "in the program"
                     . T.replace "[GHC.Types.Char] -> " ""
                     . T.replace "base:GHC.Base.String -> " ""
                     . T.replace "IO ()" "Program"
                     . T.replace "IO t0" "Program"
                     . T.replace "Perhaps you intended to use TemplateHaskell" ""
                     . T.replace "(imported from Prelude)" ""
                     . T.filter isAscii
                     . T.decodeUtf8
