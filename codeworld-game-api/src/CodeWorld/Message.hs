{-# LANGUAGE DeriveGeneric #-}

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

      <- Started

  Client sends an input event.  The server sends input event to all players, with a player id.

      -> InEvent <e>
      <- OutEvent <i> <e>

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
    | InEvent String
    deriving (Show, Read)

data ServerMessage
    = JoinedAs PlayerId GameId
    | PlayersWaiting Int Int
    | Started
    | OutEvent PlayerId String
    | GameAborted
    deriving (Show, Read, Eq) -- Eq is only for testing
