{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
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

import CodeWorld.Message
import CodeWorld

import System.Clock
import Data.List
import Data.Maybe
import Text.Read
import Control.Monad
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Network.WebSockets as WS
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Options.Applicative
import Text.Regex

connect :: Config -> WS.ClientApp a -> IO a
connect Config {..} = WS.runClient hostname port path

sendClientMessage :: WS.Connection -> ClientMessage -> IO ()
sendClientMessage conn msg = WS.sendTextData conn (T.pack (show msg))

type Timestamp = Double

encodeEvent :: (Timestamp, Maybe Event) -> String
encodeEvent = show

decodeEvent :: String -> Maybe (Timestamp, Maybe Event)
decodeEvent = readMaybe



getServerMessage :: WS.Connection -> IO ServerMessage
getServerMessage conn = do
    msg <- WS.receiveData conn
    case readMaybe (T.unpack msg) of
        Just msg -> return msg
        Nothing -> fail "Invalid server message"

joinGame :: Config -> IO [ServerMessage]
joinGame config = do
    connect config $ \conn -> do
        sendClientMessage conn (JoinGame (gameId config) "BOT")
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
    startTime <- getTime Monotonic
    forever $ do
        OutEvent pid eo <- getServerMessage conn
        when (pid == 0) $ do
            case decodeEvent eo of
                Just (t,mbEvent) -> do
                    let mbEvent' = modify <$> mbEvent
                    currentTime <- getTime Monotonic
                    let t' | Just ms <- delay config = max 0 (t + ms/1000)
                           | otherwise               = timeSpecToS (currentTime - startTime)
                    sendClientMessage conn (InEvent (show (t',mbEvent')))
                Nothing -> putStrLn $ "Could not parse event: " ++ eo
  where
    modify e | not (invert config) = e
    modify (KeyPress d) = KeyPress (inv d)
    modify (KeyRelease d) = KeyRelease (inv d)
    modify e = e

    inv "Up"    = "Down"
    inv "Down"  = "Up"
    inv "Left"  = "Right"
    inv "Right" = "Left"

timeSpecToS ts = fromIntegral (sec ts) + fromIntegral (nsec ts) * 1E-9

data Config = Config
    { clients  :: Int
    , invert   :: Bool
    , delay    :: Maybe Double
    , hostname :: String
    , port     :: Int
    , path     :: String
    , gameId   :: GameId
    }

opts = info (helper <*> config)
      ( fullDesc
     <> progDesc "CodeWorld simple bot"
     <> header "codeword-game-bot - a simple mirroring bot for codeworld-gameserver")
  where
    config :: Parser Config
    config = Config
      <$> option auto
          ( long "clients"
         <> short 'c'
         <> showDefault
         <> metavar "N"
         <> value 1
         <> help "Number of clients to simulate (>=1)" )
      <*> switch
          ( long "invert"
         <> showDefault
         <> help "Return opposite direction" )
      <*> optional (option auto
          ( long "delay"
         <> showDefault
         <> metavar "ms"
         <> help "Use remote timestamp and adjust with this many milli seconds. Default is to use local time stamps. Can be negative."))
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
         <> help "Path")
      <*> (T.pack <$> strOption
          ( long "gameid"
         <> showDefault
         <> metavar "ID"
         <> help "The ID of the game to join (4 letters)"))

main = do
  config <- execParser opts
  start <- getTime Monotonic
  mapConcurrently id $ replicate (clients config) (joinGame config)
