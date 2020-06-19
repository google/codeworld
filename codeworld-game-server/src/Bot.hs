{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import CodeWorld
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
import CodeWorld.Message
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Options.Applicative
import System.Clock
import Text.Read
import Text.Regex
import qualified Wuss as Wuss

connect :: Config -> WS.ClientApp a -> IO a
connect (Config {..})
  | secure =
    Wuss.runSecureClientWith
      hostname
      (fromIntegral port)
      path
      WS.defaultConnectionOptions
      [("Host", BS.pack hostname)]
  | otherwise =
    WS.runClientWith
      hostname
      port
      path
      WS.defaultConnectionOptions
      [("Host", BS.pack hostname)]

type Timestamp = Double

encodeEvent :: (Timestamp, Maybe Event) -> String
encodeEvent = show

decodeEvent :: String -> Maybe (Timestamp, Maybe Event)
decodeEvent = readMaybe

sendClientMessage :: Config -> WS.Connection -> ClientMessage -> IO ()
sendClientMessage config conn msg = do
  when (debug config) $ do
    tid <- myThreadId
    putStrLn $ show tid ++ " → " ++ show msg
  WS.sendTextData conn (T.pack (show msg))

getServerMessage :: Config -> WS.Connection -> IO ServerMessage
getServerMessage config conn = do
  msg <- WS.receiveData conn
  case readMaybe (T.unpack msg) of
    Just msg -> do
      when (debug config) $ do
        tid <- myThreadId
        putStrLn $ show tid ++ " ← " ++ show msg
      return msg
    Nothing -> fail "Invalid server message"

run :: Config -> MVar (Maybe GameId) -> IO [ServerMessage]
run config game = do
  mgame <- takeMVar game
  case mgame of
    Just gameid -> do
      putMVar game (Just gameid)
      joinGame config gameid
    Nothing -> do
      newGame config game

newGame :: Config -> MVar (Maybe GameId) -> IO [ServerMessage]
newGame config game =
  connect config $ \conn -> do
    sendClientMessage config conn (NewGame (clients config) "BOT")
    JoinedAs _ gameid <- getServerMessage config conn
    putMVar game (Just gameid)
    waitForStart config conn

joinGame :: Config -> GameId -> IO [ServerMessage]
joinGame config gameId =
  connect config $ \conn -> do
    sendClientMessage config conn (JoinGame gameId "BOT")
    JoinedAs _ _ <- getServerMessage config conn
    waitForStart config conn

waitForStart :: Config -> WS.Connection -> IO [ServerMessage]
waitForStart config conn = go
  where
    go = do
      m <- getServerMessage config conn
      case m of
        Started {} -> playGame config conn
        _ -> go

playGame :: Config -> WS.Connection -> IO [ServerMessage]
playGame config conn = do
  startTime <- getTime Monotonic
  forever $ do
    OutEvent pid eo <- getServerMessage config conn
    when (pid == 0) $
      case decodeEvent eo of
        Just (t, mbEvent) -> do
          let mbEvent' = modify <$> mbEvent
          currentTime <- getTime Monotonic
          let t'
                | Just ms <- delay config = max 0 (t + ms / 1000)
                | otherwise = timeSpecToS (currentTime - startTime)
          sendClientMessage
            config
            conn
            (InEvent (show (t', mbEvent')))
        Nothing -> putStrLn $ "Could not parse event: " ++ eo
  where
    modify e
      | not (invert config) = e
    modify (KeyPress d) = KeyPress (inv d)
    modify (KeyRelease d) = KeyRelease (inv d)
    modify e = e
    inv "Up" = "Down"
    inv "Down" = "Up"
    inv "Left" = "Right"
    inv "Right" = "Left"
    inv c = c

timeSpecToS ts = fromIntegral (sec ts) + fromIntegral (nsec ts) * 1E-9

data Config = Config
  { clients :: Int,
    invert :: Bool,
    secure :: Bool,
    delay :: Maybe Double,
    hostname :: String,
    port :: Int,
    path :: String,
    gameId :: Maybe GameId,
    debug :: Bool
  }

opts =
  info
    (helper <*> config)
    ( fullDesc <> progDesc "CodeWorld simple bot"
        <> header
          "codeword-game-bot - a simple mirroring bot for codeworld-gameserver"
    )
  where
    config :: Parser Config
    config =
      Config
        <$> option
          auto
          ( long "clients" <> short 'c' <> showDefault <> metavar "N"
              <> value 1
              <> help "Number of clients to simulate (>=1)"
          )
        <*> switch
          (long "invert" <> showDefault <> help "Return opposite direction")
        <*> switch
          (long "secure" <> short 's' <> help "Use a secure connection")
        <*> optional
          ( option
              auto
              ( long "delay" <> showDefault <> metavar "ms"
                  <> help
                    "Use remote timestamp and adjust with this many milli seconds. Default is to use local time stamps. Can be negative."
              )
          )
        <*> strOption
          ( long "hostname" <> showDefault <> value "0.0.0.0"
              <> metavar "HOSTNAME"
              <> help "Hostname"
          )
        <*> option
          auto
          ( long "port" <> showDefault <> metavar "PORT" <> value 9160
              <> help "Port"
          )
        <*> strOption
          ( long "path" <> showDefault <> metavar "PATH" <> value "/gameserver"
              <> help "Path"
          )
        <*> optional
          ( strOption
              ( long "gameid" <> showDefault <> metavar "ID"
                  <> help "The ID of the game to join (4 letters)"
              )
          )
        <*> switch (long "debug" <> showDefault <> help "Show debugging output")

main = do
  config <- execParser opts
  start <- getTime Monotonic
  game <- newMVar (gameId config)
  mapConcurrently id $ replicate (clients config) (run config game)
