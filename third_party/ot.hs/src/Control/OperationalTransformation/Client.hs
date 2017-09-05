module Control.OperationalTransformation.Client
  ( ClientState (..)
  , initialClientState
  , applyClient
  , applyServer
  , serverAck
  ) where

import Control.OperationalTransformation

-- | At every moment, the client is in one of three states.
data ClientState op
  -- | All of the client's operations have been acknowledged by the server.
  = ClientSynchronized
  -- | The client has sent an operation to the server and it is still waiting
  -- for an acknowledgement.
  | ClientWaiting op
  -- | The client is waiting for an acknowledgement for a pending operation and
  -- the client is buffering local changes.
  | ClientWaitingWithBuffer op op
  deriving (Eq, Show, Read)

-- | The state a newly connected client starts in (synonym for
-- 'ClientSynchronized').
initialClientState :: ClientState op
initialClientState = ClientSynchronized

-- | Handle user-generated operations.
applyClient :: (OTComposableOperation op)
            => ClientState op
            -> op
            -- ^ The operation the user has performed on the local document.
            -> Either String (Bool, ClientState op)
            -- ^ Whether to send the operation to the server and the new client
            -- state (or an error).
applyClient ClientSynchronized op = Right (True, ClientWaiting op)
applyClient (ClientWaiting w) op = Right (False, ClientWaitingWithBuffer w op)
applyClient (ClientWaitingWithBuffer w b) op = case compose b op of
  Left err -> Left $ "operations couldn't be composed: " ++ err
  Right b' -> Right (False, ClientWaitingWithBuffer w b')

-- | Handle incoming operations from the server.
applyServer :: (OTComposableOperation op)
            => ClientState op
            -> op
            -> Either String (op, ClientState op)
            -- ^ The transformed operation that must be applied to local
            -- document and the new state (or an error).
applyServer ClientSynchronized op = Right (op, ClientSynchronized)
applyServer (ClientWaiting w) op = case transform w op of
  Left err -> Left $ "transform failed: " ++ err
  Right (w', op') -> Right (op', ClientWaiting w')
applyServer (ClientWaitingWithBuffer w b) op = case transform w op of
  Left err -> Left $ "transform failed: " ++ err
  Right (w', op') -> case transform b op' of
    Left err -> Left $ "transform failed: " ++ err
    Right (b', op'') -> Right (op'', ClientWaitingWithBuffer w' b')

-- | Handle acknowledgements.
serverAck :: ClientState op
          -> Maybe (Maybe op, ClientState op)
          -- ^ An optional operation that must be sent to the server if present
          -- and the new state.
serverAck ClientSynchronized            = Nothing
serverAck (ClientWaiting _)             = Just (Nothing, ClientSynchronized)
serverAck (ClientWaitingWithBuffer _ b) = Just (Just b, ClientWaiting b)