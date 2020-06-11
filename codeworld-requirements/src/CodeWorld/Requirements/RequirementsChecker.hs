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

module CodeWorld.Requirements.RequirementsChecker (plugin) where

import CodeWorld.Requirements.Framework
import CodeWorld.Requirements.Requirements
import qualified Data.ByteString as B
import Data.Text.Encoding
import ErrUtils
import HscTypes
import Outputable
import Plugins
import TcRnMonad

plugin :: Plugin
plugin =
  defaultPlugin
    { renamedResultAction = keepRenamedSource,
      typeCheckResultAction = \_args -> requirementsChecker
    }

requirementsChecker :: ModSummary -> TcGblEnv -> TcM TcGblEnv
requirementsChecker summary env = do
  src <- liftIO (B.readFile $ ms_hspp_file summary)

  let flags = ms_hspp_opts summary
      parsed = ghcParseCode flags [] $ decodeUtf8 src

  case parsed of
    GHCParsed code -> do
      lcl <- getLclEnv
      errs <- readTcRef $ tcl_errs lcl
      let req = checkRequirements flags errs env code src

      case req of
        Nothing -> return ()
        Just r -> liftIO (putMsg flags $ text r)
    GHCNoParse -> return ()

  pure env
