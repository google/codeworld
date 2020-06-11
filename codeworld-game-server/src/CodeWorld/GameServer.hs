{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

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
module CodeWorld.GameServer
  ( ServerState,
    initGameServer,
    gameServer,
    gameStats,
  )
where

import CodeWorld.Message
import Control.Applicative
import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isPunctuation, isSpace)
import qualified Data.HashMap.Strict as HM
import Data.List (find)
import Data.Monoid ((<>), mappend)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import GHC.Generics
import GHC.Stats
import qualified Network.WebSockets as WS
import Network.WebSockets.Snap
import Snap.Core (MonadSnap, extendTimeout, modifyResponse, setHeader, writeLBS)
import System.Random
import Text.Read

-- Server state
type Key = (Signature, GameId)

data Game = Game
  { numPlayers :: Int,
    gameKey :: Key,
    gameState :: GameState,
    players :: [(PlayerId, WS.Connection)],
    -- | counts only broadcasted game message
    gameEventCount :: !Int,
    gameEventSize :: !Int
  }

data GameState
  = Waiting
  | Running

type Games = HM.HashMap Key (MVar Game)

data ServerState = ServerState
  { games :: MVar Games,
    totalStats :: MVar TotalStats,
    started :: UTCTime
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
          let game =
                Game
                  { numPlayers = playerCount,
                    gameKey = (sig, gid),
                    gameState = Waiting,
                    players = [],
                    gameEventCount = 0,
                    gameEventSize = 0
                  }
          gameMV <- newMVar game
          return (HM.insert (sig, gid) gameMV games, (gid, gameMV))

joinGame :: WS.Connection -> MVar Game -> IO (Maybe PlayerId)
joinGame conn gameMV =
  modifyMVar gameMV $ \game ->
    case game of
      Game {gameState = Waiting}
        | length (players game) < numPlayers game ->
          let Just pid =
                find (`notElem` map fst (players game)) [0 ..] -- fill holes
              game' = game {players = (pid, conn) : players game}
           in return (game', Just pid)
      _ -> return (game, Nothing)

tryStartGame :: MVar Game -> IO Bool
tryStartGame gameMV =
  modifyMVar gameMV $ \game ->
    case game of
      Game {gameState = Waiting}
        | length (players game) == numPlayers game ->
          return (game {gameState = Running}, True)
      _ -> return (game, False)

getPlayers :: MVar Game -> IO [WS.Connection]
getPlayers gameMVar = map snd . players <$> readMVar gameMVar

getStats :: MVar Game -> IO (Int, Int)
getStats gameMVar = go <$> readMVar gameMVar
  where
    go game = (length (players game), numPlayers game)

cleanup :: MVar Game -> PlayerId -> ServerState -> IO ()
cleanup gameMV mypid state = do
  done <- modifyMVar gameMV go
  if done
    then do
      game <- readMVar gameMV
      let key = gameKey game
      modifyMVar_ (games state) $ return . HM.delete key
      modifyMVar_ (totalStats state) $ \ts ->
        return
          $! ts
            { totalEventCount = totalEventCount ts + gameEventCount game,
              totalEventSize = totalEventSize ts + gameEventSize game
            }
    else do
      announcePlayers gameMV
  where
    go g =
      let players' = filter ((/= mypid) . fst) (players g)
       in return (g {players = players'}, null players')

-- Communication
sendServerMessage :: ServerMessage -> WS.Connection -> IO ()
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
    forM_ (players game) (\(_, conn) -> WS.sendTextData conn msg_txt)
  modifyMVar_ gameMV $ \game ->
    return
      $! game
        { gameEventCount = gameEventCount game + 1,
          gameEventSize = gameEventSize game + BS.length msg_txt
        }

-- Statistics
data CurrentStats = CurrentStats
  { waitingGames :: !Int,
    runningGames :: !Int,
    connections :: !Int
  }
  deriving (Show, Generic)

instance ToJSON CurrentStats

data TotalStats = TotalStats
  { totalConnections :: !Int,
    totalGames :: !Int,
    totalEventCount :: !Int,
    totalEventSize :: !Int
  }
  deriving (Show, Generic)

instance ToJSON TotalStats

tickConnection :: ServerState -> IO ()
tickConnection state =
  modifyMVar_ (totalStats state) $ \ts ->
    return $! ts {totalConnections = totalConnections ts + 1}

tickGame :: ServerState -> IO ()
tickGame state =
  modifyMVar_ (totalStats state) $ \ts ->
    return $! ts {totalGames = totalGames ts + 1}

deriving instance Generic RTSStats

deriving instance Generic GCDetails

instance ToJSON RTSStats

instance ToJSON GCDetails

data ServerStats
  = ServerStats
      CurrentStats
      TotalStats
      RTSStats

-- | merge the fields of 'CurrentStats' and 'TotalStats'
instance ToJSON ServerStats where
  toJSON (ServerStats cs ts gs) = Object (o1 <> o2 <> o3)
    where
      Object o1 = toJSON cs
      Object o2 = toJSON ts
      Object o3 = object ["mem" .= toJSON gs]

allGames :: ServerState -> IO [Game]
allGames state = do
  gm <- readMVar (games state)
  mapM readMVar $ HM.elems gm

tally :: [Game] -> CurrentStats
tally games = CurrentStats {..}
  where
    waitingGames = length [() | Game {gameState = Waiting {}} <- games]
    runningGames = length [() | Game {gameState = Running {}} <- games]
    connections = sum [length (players g) | g <- games]

gameStats :: MonadSnap m => ServerState -> m ()
gameStats state = do
  cs <- tally <$> liftIO (allGames state)
  ts <- liftIO $ readMVar (totalStats state)
  gs <- liftIO $ getRTSStats
  let stats = ServerStats cs ts gs
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS (encode stats)

-- Handling logic

-- | Initializes the mutable state of the game server
initGameServer :: IO ServerState
initGameServer = do
  started <- getCurrentTime
  totalStats <- newMVar (TotalStats 0 0 0 0)
  games <- newMVar HM.empty
  return (ServerState {..})

-- | A snap handler
gameServer :: MonadSnap m => ServerState -> m ()
gameServer state =
  -- extendTimeout 36000
  do
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
  case msg of
    NewGame n sig -> welcomeNew conn state n sig
    JoinGame gid sig -> welcomeJoin conn state gid sig

welcomeNew :: WS.Connection -> ServerState -> Int -> Signature -> IO ()
welcomeNew conn state n sig = do
  tickGame state
  (gid, gameMV) <- freshGame state n sig
  Just pid <- joinGame conn gameMV
  sendServerMessage (JoinedAs pid gid) conn
  announcePlayers gameMV
  talk pid conn gameMV `finally` cleanup gameMV pid state

findGame :: ServerState -> GameId -> Signature -> IO (MVar Game)
findGame state gid "BOT" = do
  games <- readMVar (games state)
  let (gameMV : _) = [gameMV | ((_, gid), gameMV) <- HM.toList games]
  return gameMV
findGame state gid sig = do
  Just gameMV <- HM.lookup (sig, gid) <$> readMVar (games state)
  return gameMV

welcomeJoin :: WS.Connection -> ServerState -> GameId -> Signature -> IO ()
welcomeJoin conn state gid sig = do
  gameMV <- findGame state gid sig
  Just pid <- joinGame conn gameMV
  sendServerMessage (JoinedAs pid gid) conn
  announcePlayers gameMV
  talk pid conn gameMV `finally` cleanup gameMV pid state

announcePlayers :: MVar Game -> IO ()
announcePlayers gameMV = do
  (n, m) <- getStats gameMV
  started <- tryStartGame gameMV
  broadcast
    ( if started
        then Started
        else PlayersWaiting n m
    )
    gameMV

talk :: PlayerId -> WS.Connection -> MVar Game -> IO ()
talk pid conn gameMV =
  forever $
    getClientMessage conn >>= \case
      InEvent e -> do
        g <- readMVar gameMV
        case g of
          Game {gameState = Running, ..} ->
            broadcast (OutEvent pid e) gameMV
          _ -> return ()
