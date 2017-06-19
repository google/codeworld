{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

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

module Build where

import qualified "codeworld-compiler" Main as M
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process
import           Text.Regex.TDFA

import Util

compileIfNeeded :: BuildMode -> ProgramId -> IO Bool
compileIfNeeded mode programId = do
    hasResult <- doesFileExist (buildRootDir mode </> resultFile programId)
    hasTarget <- doesFileExist (buildRootDir mode </> targetFile programId)
    if hasResult then return hasTarget else M.compileSource (buildRootDir mode </> sourceFile programId) (buildRootDir mode </> targetFile programId) (buildRootDir mode </> resultFile programId) "codeworld"

