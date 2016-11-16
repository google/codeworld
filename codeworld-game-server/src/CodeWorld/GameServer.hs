{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

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
import Control.Exception (finally)
import Control.Monad
import Control.Concurrent
import Data.Time.Clock
import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Snap.Core (MonadSnap, writeLBS, modifyResponse, setHeader, extendTimeout)
import Network.WebSockets.Snap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import System.Random
import Text.Read
import Control.Monad.IO.Class
import Control.Applicative


-- Server state

type Key = (Signature, GameId)

data Game = Game
        { numPlayers     :: Int
        , gameKey        :: Key
        , gameState      :: GameState
        , players        :: [(PlayerId, WS.Connection)]
        , gameEventCount :: !Int -- ^ counts only broadcasted game message
        , gameEventSize  :: !Int
        }

data GameState = Waiting | Running { startTime :: UTCTime }

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
            let game = Game
                    { numPlayers     = playerCount
                    , gameKey        = (sig, gid)
                    , gameState      = Waiting
                    , players        = []
                    , gameEventCount = 0
                    , gameEventSize  = 0
                    }
            gameMV <- newMVar game
            return (HM.insert (sig, gid) gameMV games, (gid, gameMV))


joinGame :: WS.Connection -> MVar Game -> IO (Maybe PlayerId)
joinGame conn gameMV = modifyMVar gameMV $ \game -> case game of
        Game { gameState = Waiting } | length (players game) < numPlayers game ->
                let pid = length (players game)
                    game' = game { players = (pid, conn) : players game }
                in return (game', Just pid)
        _ -> return (game, Nothing)

tryStartGame :: MVar Game -> IO Bool
tryStartGame gameMV = modifyMVar gameMV $ \game -> case game of
        Game { gameState = Waiting } | length (players game) == numPlayers game -> do
                time <- getCurrentTime
                return (game { gameState = Running time} , True)
        _ -> return (game, False)

getPlayers :: MVar Game -> IO [WS.Connection]
getPlayers gameMVar = map snd . players <$> readMVar gameMVar

getStats :: MVar Game -> IO (Int, Int)
getStats gameMVar = go <$> readMVar gameMVar
  where go game = (length (players game), numPlayers game)

cleanup :: MVar Game -> PlayerId -> ServerState -> IO ()
cleanup gameMV mypid state = do
    done <- modifyMVar gameMV go
    when done $ do
        game <- readMVar gameMV
        let key = gameKey game
        modifyMVar_ (games state) $ return . HM.delete key
        modifyMVar_ (totalStats state) $ \ts -> return $!
            ts { totalEventCount = totalEventCount ts + gameEventCount game
               , totalEventSize  = totalEventSize ts  + gameEventSize game
               }
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
broadcast msg gameMV = do
    let !msg_txt = T.encodeUtf8 (T.pack (show msg))
    withMVar gameMV $ \game ->
        forM_ (players game) (\(_,conn) -> WS.sendTextData conn msg_txt)
    modifyMVar_ gameMV $ \game -> return $!
        game { gameEventCount = gameEventCount game + 1
             , gameEventSize = gameEventSize game + BS.length msg_txt
             }


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
    , totalEventCount  :: !Int
    , totalEventSize   :: !Int
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
    waitingGames = length [ () | Game { gameState = Waiting {}} <- games ]
    runningGames = length [ () | Game { gameState = Running {}} <- games ]
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
    totalStats <- newMVar (TotalStats 0 0 0 0)
    games <- newMVar HM.empty
    return $ ServerState {..}


-- | A snap handler
gameServer :: MonadSnap m => ServerState -> m ()
gameServer state = do
    -- extendTimeout 36000
    runWebSocketsSnap (wsApp state)

wsApp :: ServerState -> WS.ServerApp
wsApp state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    welcome conn state

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
        Game { gameState = Running{..}, ..} -> do
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
            Game { gameState = Running{..}, ..} -> do
                let time = realToFrac (diffUTCTime currentTime startTime)
                broadcast (OutEvent time pid e) gameMV
            _           -> return ()
    InPing time -> do
        broadcast (OutPing time pid) gameMV
