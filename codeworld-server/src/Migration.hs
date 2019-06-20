{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import Util

getAllUserDirs :: BuildMode -> IO [FilePath]
getAllUserDirs bm = listDirectoryWithPrefix $ projectRootDir bm

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

main :: IO ()
main = do
    putStrLn "Running migration of codeworld-server project structure."
    alreadyDone <- (&&)
      <$> isProjectStructureCorrect (projectRootDir (BuildMode "codeworld"))
      <*> isProjectStructureCorrect (projectRootDir (BuildMode "haskell"))
    if alreadyDone then putStrLn "Migration already done."
        else do
            error "Migration should be done"
            migrateMode (BuildMode "haskell")
            migrateMode (BuildMode "codeworld")
            migrateMode (BuildMode "blocklyXML")
            putStrLn "Successfully migrated."
