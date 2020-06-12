{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

{-
  Copyright 2020 The CodeWorld Authors. All rights reserved.

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

import Control.Monad
import Data.ByteString (ByteString)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import GHC.PackageDb
import System.Environment
import System.Exit

instance BinaryStringRep ByteString where
  fromStringRep = id
  toStringRep = id

type BSPkg =
  InstalledPackageInfo
    ByteString
    ByteString
    ByteString
    ByteString
    BSUnitId
    ByteString
    BSModule

newtype BSModule
  = BSModule
      (DbModule ByteString ByteString BSUnitId ByteString BSModule)
  deriving (Show)

newtype BSUnitId
  = BSUnitId
      (DbUnitId ByteString ByteString BSUnitId ByteString BSModule)
  deriving (Show)

instance
  DbUnitIdModuleRep
    ByteString
    ByteString
    BSUnitId
    ByteString
    BSModule
  where
  fromDbModule = BSModule
  toDbModule (BSModule mod) = mod
  fromDbUnitId = BSUnitId
  toDbUnitId (BSUnitId uid) = uid

getModuleMap :: BSPkg -> [(ByteString, [ByteString])]
getModuleMap pkg@(InstalledPackageInfo {..}) =
  [ (modname, [uid])
    | exposed,
      (modname, mod) <- exposedModules,
      Just uid <- [modToUnitId mod]
  ]
  where
    modToUnitId Nothing = Just unitId
    modToUnitId (Just (BSModule (DbModule uid _))) = toUnitId uid
    modToUnidId _ = Nothing
    toUnitId (BSUnitId (DbInstalledUnitId uid)) = Just uid
    toUnitId _ = error "TODO"

main :: IO ()
main = do
  paths <- getArgs
  dbs <- mapM readPackageDbForGhc paths
  let fulldb = concat dbs
  let modAssocs = concatMap getModuleMap fulldb
  let modMap = M.fromListWith (++) modAssocs
  let badMods = M.filter ((> 1) . length) (nub <$> modMap)
  forM_ (M.assocs badMods) $ \(m, uids) -> do
    putStrLn $ "Module " ++ show m ++ " provided by multiple pkgs:"
    forM_ uids $ \uid -> putStrLn $ "  " ++ show uid
  when (not (null badMods)) $ exitWith (ExitFailure 1)
