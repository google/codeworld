{-# LANGUAGE OverloadedStrings #-}

module Main where

import Util
import System.Directory
import System.FilePath
import System.File.Tree (FSTree, getDirectory, flatten)

getAllUserDirs :: BuildMode -> IO [FilePath]
getAllUserDirs bm = listDirectoryWithPrefix $ projectRootDir bm

-- Projects directories have no 3-letters prefix dirs inside.
isProjectsStructureCorrect :: IO Bool
isProjectsStructureCorrect = do
    let cwDir = projectRootDir (BuildMode "codeworld")
        haskellDir = projectRootDir (BuildMode "haskell")
    cwTree <- getDirectory cwDir
    haskellTree <- getDirectory haskellDir
    return $ checkForPrefDirs haskellTree && checkForPrefDirs cwTree
    where
        checkForPrefDirs :: FSTree -> Bool
        checkForPrefDirs tr = null $ filter ((\x -> 3 == length x)) $ concatMap splitPath $ flatten tr

move :: FilePath -> FilePath -> IO ()
move src dest = do
    isdir <- isDir src
    case isdir of
        True  -> renameDirectory src dest
        False -> renameFile src dest

migrateProjectStructure :: FilePath -> IO ()
migrateProjectStructure path = do
    isdir <- isDir path
    children <- if isdir then listDirectory path else return []
    let parent = takeDirectory path
        sources = map (path </>) children
        destinations = map (parent </>) children
    case isdir of
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

main :: IO ()
main = do
    print "Running migration of codeworld-server project structure."
    alreadyDone <- isProjectsStructureCorrect
    if alreadyDone then print "Migration already done."
        else do
            migrateMode (BuildMode "haskell")
            migrateMode (BuildMode "codeworld")
            print "Succsessfully migrated."
