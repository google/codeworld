{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

{-
  Copyright 2016 The CodeWorld Authors. All rights reserved.

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

import CodeWorld.Message

import System.Clock
import Data.List
import Text.Read
import Control.Monad
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Network.WebSockets as WS
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Options.Applicative

connect :: Config -> WS.ClientApp a -> IO a
connect Config {..} = WS.runClient hostname port path

sendClientMessage :: ClientMessage -> WS.Connection ->  IO ()
sendClientMessage msg conn = WS.sendTextData conn (T.pack (show msg))

getServerMessage :: WS.Connection -> IO ServerMessage
getServerMessage conn = do
    msg <- WS.receiveData conn
    case readMaybe (T.unpack msg) of
        Just msg -> return msg
        Nothing -> fail "Invalid server message"

joinGame :: Config -> GameId -> IO [ServerMessage]
joinGame config gid = do
    connect config $ \conn -> do
        sendClientMessage (JoinGame gid sig) conn
        JoinedAs _ _ <- getServerMessage conn
        waitForStart config conn

waitForStart :: Config -> WS.Connection -> IO [ServerMessage]
waitForStart config conn = go
  where
    go = do
        m <- getServerMessage conn
        case m of
            Started {} -> playGame config conn
            _ -> go

playGame :: Config -> WS.Connection -> IO [ServerMessage]
playGame config conn = do
    forkIO $ sendMessages config conn
    getAllMessages config conn

sendMessages :: Config -> WS.Connection -> IO ()
sendMessages config conn = do
    forM_ [1..events config] $ \n -> do
        sendClientMessage (InEvent (show n)) conn

getAllMessages :: Config -> WS.Connection -> IO [ServerMessage]
getAllMessages config conn =
    replicateM (nrequests config) (getServerMessage conn) <*
    WS.sendClose conn BS.empty

timeSpecToS ts = fromIntegral (sec ts) + fromIntegral (nsec ts) * 1E-9

data Config = Config
    { clients  :: Int
    , events   :: Int
    , hostname :: String
    , port     :: Int
    , path     :: String
    }

opts = info (helper <*> config)
      ( fullDesc
     <> progDesc "CodeWorld gameserver stresstest client"
     <> header "codeword-game-stresstest - a stresstest for codeworld-gameserver")
  where
    config :: Parser Config
    config = Config
      <$> option auto
          ( long "clients"
         <> short 'c'
         <> showDefault
         <> metavar "N"
         <> value 3
         <> help "Number of clients (>=1)" )
      <*> option auto
          ( long "events"
         <> short 'e'
         <> showDefault
         <> metavar "M"
         <> value 100
         <> help "Number of events every client should send" )
      <*> strOption
          ( long "hostname"
         <> showDefault
         <> value "0.0.0.0"
         <> metavar "HOSTNAME"
         <> help "Hostname" )
      <*> option auto
          ( long "port"
         <> showDefault
         <> metavar "PORT"
         <> value 9160
         <> help "Port" )
      <*> strOption
          ( long "path"
         <> showDefault
         <> metavar "PATH"
         <> value "gameserver"
         <> help "Path" )

main = do
  config <- execParser opts
  start <- getTime Monotonic
  connect config $ \conn -> do
    sendClientMessage (NewGame (clients config) sig) conn
    JoinedAs 0 gid <- getServerMessage conn
    results <- mapConcurrently id $
        waitForStart config conn : replicate (clients config - 1) (joinGame config gid)
    end <- getTime Monotonic

    let consistent = all (== head results) (tail results)
    if consistent then putStrLn "All clients got consistent data."
                  else putStrLn "The clients got different results!"
    putStrLn $ "Events sent:         " ++ show (nrequests config)
    putStrLn $ "Running time was:    " ++ show (timeSpecToS (end-start) * 1000) ++ "ms"
    putStrLn $ "Requests per second: " ++ show (fromIntegral (nrequests config) / timeSpecToS (end-start))

sig :: BS.ByteString
sig = BS.pack "DemoGame"


nrequests config = clients config * events config
