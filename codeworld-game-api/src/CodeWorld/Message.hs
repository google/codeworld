{-# LANGUAGE DeriveGeneric #-}

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

module CodeWorld.Message where

import Data.Text (Text)

{-
  Protocol:

  Create a new game, join it as player 0 and waits for n players in total.
  Returns an id for the new game.

      -> NewGame <m>
      <- GameCreated <gid>

  Joins an existing game. Returns the player id, the number of players.

      -> JoinGame <gid>
      <- JoinedAs <i>

  Server tells the clients about the number of connected players and number of expected players

      <- PlayersWaiting <n> <m>

  Server tells the clients that the game has started

      <- Started <timestamp>

  Server sends periodic pings to let clients know an updated committed timestamp.

      <- Ping <timestamp>

  Client sends an input event.  The server sends input event to all players, with a player id
  and timestamp (seconds).  Currently, the client timestamp is ignored, and the server attaches
  a new server timestamp to all messages.

      -> InEvent <timestamp> <e>
      <- OutEvent <timestamp> <i> <e>

  Clients can send periodic pings to update with their time.  The server repeats them.
  Currently, other clients ignore these pings.

      -> InPing <timestamp>
      -> OutPing <timestamp> <i>

  Server indicates to the client that they have been disconnected. Closes the connection.

      <- GameAborted
-}

type GameId = Text
type PlayerId = Int

-- Message representation

data ClientMessage
    = NewGame Int
    | JoinGame GameId
    | InEvent Double String
    | InPing Double
    deriving (Show, Read)

data ServerMessage
    = GameCreated GameId
    | Ping Double
    | JoinedAs PlayerId
    | PlayersWaiting Int Int
    | Started Double
    | OutEvent Double PlayerId String
    | OutPing Double PlayerId
    | GameAborted
    deriving (Show, Read)
