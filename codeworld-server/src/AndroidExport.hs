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

import           Data.Maybe
import qualified Data.Map as M
import           System.Process
import           System.Directory
import           System.FilePath
import qualified System.IO.Strict as ST
import           Text.HTML.TagSoup

import Util

buildAndroid :: BuildMode -> ProgramId -> AppProps -> IO ()
buildAndroid mode programId appProps = do
  let appName = fromJust $ M.lookup "appName" appProps
  initCordovaProject mode programId
  copySource mode programId
  setAppName mode programId appName
  buildApk mode programId
  return ()

initCordovaProject :: BuildMode -> ProgramId -> IO ()
initCordovaProject mode programId = do
  let rootDir = androidRootDir mode
  checkIfRootExists <- doesDirectoryExist rootDir
  if not checkIfRootExists
     then do
       createDirectory $ androidRootDir mode
     else return ()
  let buildDir = androidBuildDir mode programId
  checkIfBuildExists <- doesDirectoryExist buildDir
  if not checkIfBuildExists
    then do
      checkIfParentExists <- doesDirectoryExist $ androidRootDir mode </> sourceParent programId
      if not checkIfParentExists
        then do
          createDirectory $ androidRootDir mode </> sourceParent programId
          copyDirIfExists "android-template" (androidRootDir mode </> sourceBase programId)
        else return ()
    else return ()

copySource :: BuildMode -> ProgramId -> IO ()
copySource mode programId =
  copyFile
    (buildRootDir mode </> targetFile programId)
    (androidBuildDir mode programId </> "www" </> "js" </> "runjs.js")

setAppName :: BuildMode -> ProgramId -> String -> IO ()
setAppName mode programId appName = do
  let configFileName = androidBuildDir mode programId </> "config.xml"
  configContents <- ST.readFile configFileName
  let tagSoup = parseTags configContents
  let newNameTag = [TagOpen "name" [], TagText appName]
  writeFile configFileName (renderTags $ newSoup tagSoup newNameTag)
    where newSoup soup insertTag = takeWhile nameId soup
                                ++ insertTag
                                ++ drop 2 (dropWhile nameId soup)
          nameId = (~/= ("<name>"::String))

buildApk :: BuildMode -> ProgramId -> IO ()
buildApk mode programId = do
  currwd <- getCurrentDirectory
  setCurrentDirectory $ androidBuildDir mode programId
  readProcess "cordova" ["build", "android"] ""
  setCurrentDirectory currwd
