{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

module CodeWorld.GameServer
    ( ServerState
    , initGameServer
    , gameServer
    , gameStats
    ) where

import CodeWorld.Message

import Data.Char (isPunctuation, isSpace)
import Data.Monoid ((<>), mappend)
import Data.Text (Text)
import Control.Exception (finally, catch, SomeException)
import Control.Monad
import Control.Concurrent
import Data.Time.Clock
import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Snap.Core (MonadSnap, writeLBS, modifyResponse, setHeader)
import Network.WebSockets.Snap
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import System.Random
import Text.Read
import Control.Monad.IO.Class
import Control.Applicative


-- Server state

type Key = (Signature, GameId)

data Game = Waiting { numPlayers :: Int, gameKey :: Key, players :: [(PlayerId, WS.Connection)] }
          | Running { startTime :: UTCTime, gameKey :: Key,  players :: [(PlayerId, WS.Connection)] }

type Games = HM.HashMap Key (MVar Game)

data ServerState = ServerState
    { games      :: MVar Games
    , totalStats :: MVar TotalStats
    , started    :: UTCTime
    }


-- Server state manipulation

randomGameId :: IO GameId
randomGameId = T.pack <$> replicateM 4 (randomRIO ('A', 'Z'))

freshGame :: ServerState -> Int -> Signature -> IO (GameId, MVar Game)
freshGame state playerCount sig = modifyMVar (games state) go
  where
    go games = do
        gid <- randomGameId
        if (sig, gid) `HM.member` games
          then go games
          else do
            let game = Waiting playerCount (sig, gid) []
            gameMV <- newMVar game
            return (HM.insert (sig, gid) gameMV games, (gid, gameMV))


joinGame :: WS.Connection -> MVar Game -> IO (Maybe PlayerId)
joinGame conn gameMV = modifyMVar gameMV $ \game -> case game of
        Waiting pc key plys | length plys < pc ->
                let pid = length plys
                    game' = Waiting pc key ((pid, conn) : plys)
                in return (game', Just pid)
        _ -> return (game, Nothing)

tryStartGame :: MVar Game -> IO Bool
tryStartGame gameMV = modifyMVar gameMV $ \game -> case game of
        Waiting pc key plys | length plys == pc -> do
                time <- getCurrentTime
                return (Running time key plys, True)
        _ -> return (game, False)

getPlayers :: MVar Game -> IO [WS.Connection]
getPlayers gameMVar = map snd . players <$> readMVar gameMVar

getStats :: MVar Game -> IO (Int, Int)
getStats gameMVar = go <$> readMVar gameMVar
  where go (Waiting pc _ plys) = (length plys, pc)
        go (Running t  _ plys) = (length plys, length plys)

cleanup :: MVar Game -> PlayerId -> ServerState -> IO ()
cleanup gameMV mypid state = do
    done <- modifyMVar gameMV go
    when done $ do
        key <- gameKey <$> readMVar gameMV
        modifyMVar_ (games state) (return . HM.delete key)
  where
    go g = let players' = filter ((/= mypid) . fst) (players g)
           in return $ (g { players = players' }, null players')

-- Communication

sendServerMessage :: ServerMessage -> WS.Connection ->  IO ()
sendServerMessage msg conn = WS.sendTextData conn (T.pack (show msg))

getClientMessage :: WS.Connection -> IO ClientMessage
getClientMessage conn = do
    msg <- WS.receiveData conn
    case readMaybe (T.unpack msg) of
        Just msg -> return msg
        Nothing -> fail "Invalid client message"

broadcast :: ServerMessage -> MVar Game -> IO ()
broadcast msg gameMV = withMVar gameMV $ \game ->
    forM_ (players game) (sendServerMessage msg . snd)


-- Statistics

data CurrentStats = CurrentStats
    { waitingGames :: !Int
    , runningGames :: !Int
    , connections  :: !Int
    } deriving (Show, Generic)
instance ToJSON CurrentStats

data TotalStats = TotalStats
    { totalConnections :: !Int
    , totalGames       :: !Int
    } deriving (Show, Generic)
instance ToJSON TotalStats

tickConnection :: ServerState -> IO ()
tickConnection state = modifyMVar_ (totalStats state) $ \ts ->
    return $! ts { totalConnections = totalConnections ts + 1}

tickGame :: ServerState -> IO ()
tickGame state = modifyMVar_ (totalStats state) $ \ts ->
    return $! ts { totalGames = totalGames ts + 1}

data ServerStats = ServerStats CurrentStats TotalStats
-- | merge the fields of 'CurrentStats' and 'TotalStats'
instance ToJSON ServerStats where
    toJSON (ServerStats cs ts) = Object (o1 <> o2)
      where Object o1 = toJSON cs
            Object o2 = toJSON ts

allGames :: ServerState -> IO [Game]
allGames state = do
    gm <- readMVar (games state)
    mapM readMVar $ HM.elems gm

tally :: [Game]  -> CurrentStats
tally games = CurrentStats {..}
  where
    waitingGames = length [ () | Waiting {} <- games ]
    runningGames = length [ () | Running {} <- games ]
    connections  = sum [ length (players g) | g <- games ]

gameStats :: MonadSnap m => ServerState -> m ()
gameStats state = do
    cs <- tally <$> liftIO (allGames state)
    ts <- liftIO $ readMVar (totalStats state)
    let stats = ServerStats cs ts
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS (encode stats)


-- Handling logic

-- | Initializes the mutable state of the game server
initGameServer :: IO ServerState
initGameServer = do
    started <- getCurrentTime
    totalStats <- newMVar (TotalStats 0 0)
    games <- newMVar HM.empty
    return $ ServerState {..}


-- | A snap handler
gameServer :: MonadSnap m => ServerState -> m ()
gameServer state = runWebSocketsSnap (wsApp state)

wsApp :: ServerState -> WS.ServerApp
wsApp state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    welcome conn state `catch` \e -> print (e :: SomeException)

welcome :: WS.Connection -> ServerState -> IO ()
welcome conn state = do
    tickConnection state
    msg <- getClientMessage conn
    case msg of NewGame n sig    -> welcomeNew conn state n sig
                JoinGame gid sig -> welcomeJoin conn state gid sig

welcomeNew :: WS.Connection -> ServerState -> Int -> Signature -> IO ()
welcomeNew conn state n sig = do
    tickGame state
    (gid, gameMV) <- freshGame state n sig
    Just pid <- joinGame conn gameMV
    sendServerMessage (JoinedAs pid gid) conn
    announcePlayers gameMV
    talk pid conn gameMV `finally` cleanup gameMV pid state

welcomeJoin :: WS.Connection -> ServerState -> GameId -> Signature -> IO ()
welcomeJoin conn state gid sig = do
    Just gameMV <- HM.lookup (sig, gid) <$> readMVar (games state)
    Just pid <- joinGame conn gameMV
    sendServerMessage (JoinedAs pid gid) conn
    announcePlayers gameMV
    talk pid conn gameMV `finally` cleanup gameMV pid state

announcePlayers :: MVar Game -> IO ()
announcePlayers gameMV = do
    (n, m)  <- getStats gameMV
    started <- tryStartGame gameMV
    when started $ void $ forkIO (pingThread gameMV)
    broadcast (if started then Started 0 else PlayersWaiting n m) gameMV

pingInterval :: Int
pingInterval = 1000000  -- one second

pingThread :: MVar Game -> IO ()
pingThread gameMV = do
    threadDelay pingInterval
    game <- readMVar gameMV
    currentTime <- getCurrentTime
    case game of
        Running{..} -> do
            let time = realToFrac (diffUTCTime currentTime startTime)
            broadcast (Ping time) gameMV
            unless (null players) $ pingThread gameMV
        _ -> return ()

talk ::  PlayerId -> WS.Connection -> MVar Game ->  IO ()
talk pid conn gameMV = forever $ getClientMessage conn >>= \case
    InEvent _ e -> do
        g           <- readMVar gameMV
        currentTime <- getCurrentTime
        case g of
            Running{..} -> do
                let time = realToFrac (diffUTCTime currentTime startTime)
                broadcast (OutEvent time pid e) gameMV
            _           -> return ()
    InPing time -> do
        broadcast (OutPing time pid) gameMV
