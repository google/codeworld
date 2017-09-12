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

import           Control.Applicative ((<|>))
import           Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.SocketIO as SIO
import           Network.EngineIO.Snap (snapAPI)
import           Snap.Core
import           Snap.Http.Server
import           System.Directory

import CodeWorld.GameServer
import CodeWorld.CollabServer
import SnapUtil

main :: IO ()
main = do
    hasClientId <- doesFileExist "web/clientId.txt"
    unless hasClientId $ do
        putStrLn "WARNING: Missing web/clientId.txt"
        putStrLn "User logins will not function properly!"

    clientId <- case hasClientId of
        True -> do
            txt <- T.readFile "web/clientId.txt"
            return . ClientId . Just . T.strip $ txt
        False -> do
            return $ ClientId Nothing

    gameServerState <- initGameServer
    collabServerState <- initCollabServer
    socketIOHandler <- SIO.initialize snapAPI (collabServer collabServerState clientId)
    config <- commandLineConfig $
        setPort 9160 $
        setErrorLog  (ConfigFileLog "log/collab-error.log") $
        setAccessLog (ConfigFileLog "log/collab-access.log") $
        mempty
    httpServe config $
        ifTop (gameStats gameServerState) <|>
        route [ ("gameserver", gameServer gameServerState)
              , ("socket.io" , socketIOHandler)
              ]
