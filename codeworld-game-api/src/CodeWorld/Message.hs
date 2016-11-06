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

      -> newgame <m>
      <- gameid <gid>

  Joins an existing game. Returns the player id, the number of players.

      -> joingame <gid>
      <- joined <i>

  Server tells the clients about the number of connected players and number of expected players

      -> players <n> <m>

  Server tells the clients that the game has started

      <- started

  Client sends an input event

      -> event <e>

  Server sends input event to all players, with a player id and timestamp (seconds)

      <- event <i> <timestamp> <e>

  Server indicates to the client that some other player dropped. Closes the connection.

      <- aborted
-}

type GameId = Text
type PlayerId = Int

-- Message representation

data ClientMessage
    = NewGame Int
    | JoinGame GameId
    | InEvent Double String
    deriving (Show, Read)

data ServerMessage
    = GameCreated GameId
    | Ping Double
    | JoinedAs PlayerId
    | PlayersWaiting Int Int
    | Started Double
    | OutEvent Double PlayerId String
    | GameAborted
    deriving (Show, Read)
