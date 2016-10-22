{-# LANGUAGE DeriveGeneric #-}
module CodeWorld.Message where

import CodeWorld.Event
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.UUID.Types
import Data.Scientific

type GameId = UUID
type PlayerId = Int

-- Message representation

type TimeStamp = Scientific

data ClientMessage
    = NewGame Int
    | JoinGame GameId
    | InEvent Object
    deriving Generic

data ServerMessage
    = GameCreated GameId
    | JoinedAs PlayerId
    | PlayersWaiting Int
    | Started TimeStamp
    | OutEvent TimeStamp PlayerId Object
    | GameAborted
    deriving Generic

instance FromJSON UUID where
    parseJSON x = do
        t <- parseJSON x
        case fromText t of Just uuid -> return uuid
                           Nothing   -> typeMismatch "UUID" x
instance ToJSON   UUID where
    toJSON = toJSON . toText

instance FromJSON ClientMessage where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON   ServerMessage where
    toJSON = genericToJSON defaultOptions

