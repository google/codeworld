{-# LANGUAGE DeriveGeneric #-}
module CodeWorld.Message where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.UUID.Types
import Data.Scientific

{-
Protocol:

Create a new game, join it as player 0 and waits for n players in total.
Returns an id for the new game.
→ newgame <m>
← gameid <gid>

Joins an existing game. Returns the player id, the number of players.
→ joingame <gid>
← joined <i>

Server tells the clients about the number of connected players and number of expected players
← players <n> <m>

Server tells the clients that the game has started
← started

Client sends an input event
→ event <e>

Server sends input event to all players, with a player id and timestamp (seconds)
← event <i> <timestamp> <e>

Server indicates to the client that some other player dropped. Closes the connection.
← aborted
-}

type GameId = UUID
type PlayerId = Int

-- Message representation

type TimeStamp = Scientific

data ClientMessage
    = NewGame Int
    | JoinGame GameId
    | InEvent Value
    deriving (Show, Read, Generic)

data ServerMessage
    = GameCreated GameId
    | JoinedAs PlayerId
    | PlayersWaiting Int Int
    | Started TimeStamp
    | OutEvent TimeStamp PlayerId Value
    | GameAborted
    deriving (Show, Read, Generic)

instance FromJSON UUID where
    parseJSON x = do
        t <- parseJSON x
        case fromText t of Just uuid -> return uuid
                           Nothing   -> typeMismatch "UUID" x
instance ToJSON   UUID where
    toJSON = toJSON . toText

instance FromJSON ClientMessage where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON   ClientMessage where
    toJSON = genericToJSON defaultOptions
instance FromJSON ServerMessage where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON   ServerMessage where
    toJSON = genericToJSON defaultOptions

