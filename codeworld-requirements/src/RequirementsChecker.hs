{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

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

module RequirementsChecker (plugin) where

    import Requirements
    import Control.Monad.IO.Class
    import qualified Data.ByteString as B

    import Bag
    import ErrUtils
    import HscTypes
    import Outputable
    import Plugins
    import SrcLoc
    import TcRnTypes


    plugin :: Plugin
    plugin = defaultPlugin {
        parsedResultAction = \_args -> parsedStep,
        typeCheckResultAction = \_args -> typeCheckedStep
    }

    parsedStep :: ModSummary -> HsParsedModule -> Hsc HsParsedModule
    parsedStep summary mod = do
        src <- liftIO (B.readFile $ ms_hspp_file summary)

        let flags = ms_hspp_opts summary
            (L _ code) = hpm_module mod
            req = checkRequirements flags code src

        case req of
            Nothing -> return ()
            Just r -> liftIO (putMsg flags $ text r)

        pure mod

    typeCheckedStep :: ModSummary -> TcGblEnv -> TcM TcGblEnv
    typeCheckedStep summary env = pure env