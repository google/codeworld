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

data Game = Waiting { numPlayers :: Int, signature :: Signature, players :: [(PlayerId, WS.Connection)] }
          | Running { startTime :: UTCTime, players :: [(PlayerId, WS.Connection)] }


type Games = HM.HashMap GameId Game

data ServerState = ServerState
    { games      :: MVar Games
    , totalStats :: MVar TotalStats
    , started    :: UTCTime
    }


-- Server state manipulation

randomGameId :: IO GameId
randomGameId = T.pack <$> replicateM 4 (randomRIO ('A', 'Z'))

freshGame :: ServerState -> Int -> Signature -> IO GameId
freshGame state playerCount sig = modifyMVar (games state) go
  where game = Waiting playerCount sig []
        go games = do gid <- randomGameId
                      if gid `HM.member` games
                         then go games
                         else return (HM.insert gid game games, gid)

joinGame :: WS.Connection -> GameId -> Signature -> Games -> (Games, Maybe PlayerId)
joinGame conn gameid sig games =
    case HM.lookup gameid games of
        Just (Waiting pc nsig plys) | sig == nsig && length plys < pc ->
                let pid = length plys
                    game' = Waiting pc nsig ((pid, conn) : plys)
                    games' = HM.insert gameid game' games
                in (games', Just pid)
        _ -> (games, Nothing)

tryStartGame :: GameId -> Games -> IO (Games, Bool)
tryStartGame gameid games =
    case HM.lookup gameid games of
        Just (Waiting pc _ plys) | length plys == pc -> do
                time <- getCurrentTime
                return (HM.insert gameid (Running time plys) games, True)
        _ -> return (games, False)

getPlayers :: GameId -> Games -> [WS.Connection]
getPlayers gameid games =
    case HM.lookup gameid games of
        Just game -> map snd (players game)
        Nothing   -> []

getStats :: GameId -> Games -> (Int, Int)
getStats gameid games =
    case HM.lookup gameid games of
        Just (Waiting pc _ plys) -> (length plys, pc)
        Just (Running t plys)  -> (length plys, length plys)
        Nothing   -> (0,0)

cleanup :: GameId -> PlayerId -> Games -> Games
cleanup gid mypid = HM.update cleanupGame gid
  where cleanupGame g
          | [ (pid, _) ] <- players g, pid == mypid = Nothing
          | otherwise = Just (g { players = filter ((/= mypid) . fst) (players g) })

-- Communication

sendServerMessage :: ServerMessage -> WS.Connection ->  IO ()
sendServerMessage msg conn = WS.sendTextData conn (T.pack (show msg))

getClientMessage :: WS.Connection -> IO ClientMessage
getClientMessage conn = do
    msg <- WS.receiveData conn
    case readMaybe (T.unpack msg) of
        Just msg -> return msg
        Nothing -> fail "Invalid client message"

broadcast :: ServerMessage -> GameId -> Games -> IO ()
broadcast msg gid games = forM_ (getPlayers gid games) (sendServerMessage msg)


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

tally :: Games  -> CurrentStats
tally games = CurrentStats {..}
  where
    waitingGames = length [ () | Waiting {} <- HM.elems games ]
    runningGames = length [ () | Running {} <- HM.elems games ]
    connections  = sum [ length (players g) | g <- HM.elems games ]

gameStats :: MonadSnap m => ServerState -> m ()
gameStats state = do
    cs <- tally <$> liftIO (readMVar (games state))
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


-- | A snapp handler
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
    gid <- freshGame state n sig
    Just pid <- modifyMVar (games state) (return . joinGame conn gid sig)
    sendServerMessage (JoinedAs pid gid) conn
    announcePlayers gid state
    talk pid conn gid state `finally` modifyMVar_ (games state) (return . cleanup gid pid)

welcomeJoin :: WS.Connection -> ServerState -> GameId -> Signature -> IO ()
welcomeJoin conn state gid sig = do
    Just pid <- modifyMVar (games state) (return . joinGame conn gid sig)
    sendServerMessage (JoinedAs pid gid) conn
    announcePlayers gid state
    talk pid conn gid state `finally` modifyMVar_ (games state) (return . cleanup gid pid)

announcePlayers :: GameId -> ServerState -> IO ()
announcePlayers gid state = do
    (n, m)  <- getStats gid <$> readMVar (games state)
    started <- modifyMVar (games state) (tryStartGame gid)
    when started $ void $ forkIO (pingThread gid state)
    readMVar (games state) >>=
        broadcast (if started then Started 0 else PlayersWaiting n m) gid

pingInterval :: Int
pingInterval = 1000000  -- one second

pingThread :: GameId -> ServerState -> IO ()
pingThread gid state = do
    threadDelay pingInterval
    games <- readMVar (games state)
    currentTime <- getCurrentTime
    case HM.lookup gid games of
        Just Running{..} -> do
            let time = realToFrac (diffUTCTime currentTime startTime)
            broadcast (Ping time) gid games
            when (length (getPlayers gid games) > 0) $ pingThread gid state
        _ -> return ()

talk ::  PlayerId -> WS.Connection -> GameId -> ServerState ->  IO ()
talk pid conn gid state = forever $ getClientMessage conn >>= \case
    InEvent _ e -> do
        gs          <- readMVar (games state)
        currentTime <- getCurrentTime
        case HM.lookup gid gs of
            Just Running{..} -> let time = realToFrac (diffUTCTime currentTime startTime)
                                in  readMVar (games state) >>= broadcast (OutEvent time pid e) gid
            _           -> return ()
    InPing time -> do
        readMVar (games state) >>= broadcast (OutPing time pid) gid
