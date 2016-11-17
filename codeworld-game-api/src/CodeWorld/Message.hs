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

import Data.ByteString (ByteString)
import Data.Text (Text)

{-
  Protocol:

  Create a new game with some number n of expected players, with a given signature.
  Join it automatically, and wait for other players.  Returns a player id and game id for
  the new game.

      -> NewGame <n> <sig>
      <- JoinedAs 0 <gid>

  OR join an existing game. Returns the player id and game id.  The server verifies the
  signature, and responds GameAborted if it doesn't match the signature from NewGame.

      -> JoinGame <gid> <sig>
      <- JoinedAs <i> <gid>

  Server tells the clients about the number of connected players and number of expected players

      <- PlayersWaiting <m> <n>

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
type Signature = ByteString

-- Message representation

data ClientMessage
    = NewGame Int Signature
    | JoinGame GameId Signature
    | InEvent Double String
    | InPing Double
    deriving (Show, Read)

data ServerMessage
    = JoinedAs PlayerId GameId
    | PlayersWaiting Int Int
    | Started Double
    | Ping Double
    | OutEvent Double PlayerId String
    | OutPing Double PlayerId
    | GameAborted
    deriving (Show, Read, Eq) -- Eq is only for testing
