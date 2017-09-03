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

module AndroidExport where

import System.Process
import System.Directory
import System.FilePath

import Util

buildAndroid :: BuildMode -> ProgramId -> IO()
buildAndroid mode programId = do
  initCordovaProject mode programId
  copySource mode programId
  buildApk mode programId
  return ()


initCordovaProject :: BuildMode -> ProgramId -> IO ()
initCordovaProject mode programId = do
  let buildDir = androidBuildDir mode programId
  checkIfBuildExists <- doesDirectoryExist buildDir
  case checkIfBuildExists of
    True  -> return ()
    False -> do
      checkIfParentExists <- doesDirectoryExist $ androidRootDir mode </> sourceParent programId
      case checkIfParentExists of
        True  -> return ()
        False -> do
          createDirectory $ androidRootDir mode </> sourceParent programId
          copyDirIfExists "android-template" (androidRootDir mode </> sourceBase programId)
      return ()

copySource :: BuildMode -> ProgramId -> IO ()
copySource mode programId = do
  copyFile (buildRootDir mode </> targetFile programId) (androidBuildDir mode programId </> "www" </> "js" </> "runjs.js")

buildApk :: BuildMode -> ProgramId -> IO ()
buildApk mode programId = do
  currwd <- getCurrentDirectory
  setCurrentDirectory $ androidBuildDir mode programId
  readProcess "cordova" ["build", "android"] ""
  setCurrentDirectory currwd
