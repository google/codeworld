{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Model

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import Util
import Data.List
import System.Directory
import System.FilePath
import Util

data OldProject = OldProject
     { oldProjectName :: Text
     , oldProjectSource :: Text
     , oldProjectHistory :: Value
     }

instance FromJSON OldProject where
    parseJSON (Object v) =
        OldProject <$> v .: "name" <*> v .: "source" <*> v .: "history"
    parseJSON _ = mzero

getAllUserDirs :: BuildMode -> IO [FilePath]
getAllUserDirs mode = listDirectoryWithPrefix $ projectRootDir mode

-- Project directories have no 3-letters prefix dirs inside.
isProjectStructureCorrect :: FilePath -> IO Bool
isProjectStructureCorrect path = do
    contents <- listDirectory path
    parts <- forM contents $ \f -> do
        isDir <- doesDirectoryExist (path </> f)
        if | isDir && length f == 3 && "D" `isPrefixOf` f -> return False
           | isDir -> isProjectStructureCorrect (path </> f)
           | otherwise -> return True
    return (and parts)

move :: FilePath -> FilePath -> IO ()
move src dest = do
    isDir <- doesDirectoryExist src
    case isDir of
        True  -> renameDirectory src dest
        False -> renameFile src dest

migrateProjectStructure :: FilePath -> IO ()
migrateProjectStructure path = do
    isDir <- doesDirectoryExist path
    children <- if isDir then listDirectory path else return []
    let parent = takeDirectory path
        sources = map (path </>) children
        destinations = map (parent </>) children
    case isDir of
        True -> case length (takeFileName path) == 3 of
            True -> do
                mapM_ migrateProjectStructure sources
                mapM_ (\(s, d) -> move s d) $ zip sources destinations
                removeDirectory path
            False -> do
                mapM_ migrateProjectStructure sources
        False -> return ()

migrateMode :: BuildMode -> IO ()
migrateMode bm = do
    userDirs <- getAllUserDirs bm
    mapM_ migrateProjectStructure userDirs

migrateStructure :: IO ()
migrateStructure = do
    putStrLn "Starting of structure migration."
    migrateMode (BuildMode "haskell")
    migrateMode (BuildMode "codeworld")
    migrateMode (BuildMode "blocklyXML")
    putStrLn "Structure successfully migrated."

isDirInfo "dir.info" = True
isDirInfo _ = False

isSourceFile ('S':_) = True
isSourceFile _ = False

allMetaContainers :: IO [FilePath]
allMetaContainers = do
    allFiles <- mapM getDirectory projectDirs
    return $ filterMetadata $ concat $ map flatten allFiles
    where filterMetadata paths = filter (\f -> isSourceFile f || isDirInfo f) paths

projectDirs = map projectRootDir
    [ (BuildMode "haskell")
    , (BuildMode "blocklyXML")
    , (BuildMode "codeworld")
    ]

isMetadataMigrated :: IO Bool
isMetadataMigrated = do
    files <- allMetaContainers
    -- do not traverse all filesystem for check if metadata correct
    -- on each boot
    checked <- mapM isMetaCorrect $ take 100 files
    return $ and checked      
    where
        isMetaCorrect path = do
            content <- LB.readFile path 
            case decode content :: Maybe OldProject of
                Nothing -> case decode content :: Maybe DirectoryMeta of
                    Nothing -> return False
                    _ -> return True
                _ -> return True

migrateMetadata :: IO ()
migrateMetadata = do 
    putStrLn "Starting of metadata migration."
    files <- allMetaContainers
    mapM_ migrateDirMeta $ filter isDirInfo files
    mapM_ migrateProjectMeta $ filter isSourceFile files
    putStrLn "Metadata successfully migrated."
    where 
        migrateDirMeta :: FilePath -> IO ()
        migrateDirMeta path = do
            name <- B.readFile path
            LB.writeFile path $ encode $ DirectoryMeta (T.decodeUtf8 name) 0
            return ()

        migrateProjectMeta :: FilePath -> IO ()
        migrateProjectMeta path = do
            rawMeta <- LB.readFile path
            let Just meta = decode rawMeta
            LB.writeFile path $ encode $ Project (oldProjectName meta) (oldProjectSource meta) (oldProjectHistory meta) 0
            return ()

main :: IO ()
main = do
    putStrLn "Running migration of codeworld-server."
    isMetaDone <- isMetadataMigrated
    isStructureDone <- isProjectsStructureCorrect
    case (isMetaDone, isStructureDone) of
        (True, True) -> do
            putStrLn "All migration already done"
        (False, True) -> do
            putStrLn "Metadata migration already done."
            migrateStructure
        (True, False) -> do
            putStrLn "Structure migration already done."
            migrateMetadata
        (False, False) -> do
            migrateStructure
            migrateMetadata
