module Control.OperationalTransformation.Server
  ( Revision
  , ServerState (..)
  , initialServerState
  , applyOperation
  ) where

import Control.OperationalTransformation
import Control.Monad.Trans.Either
import Control.Monad.Identity

type Revision = Integer

-- | The server keeps the current revision number and a list of previous
-- operations to transform incoming operations against.
data ServerState doc op = ServerState Revision doc [op]

initialServerState :: doc -> ServerState doc op
initialServerState doc = ServerState 0 doc []

-- | Handles incoming operations.
applyOperation :: (OTSystem doc op, OTCursor cursor op)
               => ServerState doc op
               -> Revision
               -- ^ The latest operation that the client has received from the server when it sent the operation.
               -> op
               -- ^ The operation received from the client.
               -> cursor
               -- ^ The clients cursor position after the operation. (Use @()@
               -- if not needed.)
               -> Either String (op, cursor, ServerState doc op)
               -- ^ The operation and the cursor to broadcast to all
               -- connected clients  (except the client which has created the
               -- operation; that client must be sent an acknowledgement) and
               -- the new state (or an error).
applyOperation (ServerState rev doc ops) oprev op cursor =
  runIdentity $ runEitherT $ do
    concurrentOps <- if oprev > rev || rev - oprev > fromIntegral (length ops)
      then fail "unknown revision number"
      else return $ take (fromInteger $ rev - oprev) ops
    (op', cursor') <- foldM transformFst (op, cursor) (reverse concurrentOps)
    doc' <- case apply op' doc of
      Left err -> fail $ "apply failed: " ++ err
      Right d -> return d
    return $ (op', cursor', ServerState (rev+1) doc' (op':ops))
    where
      transformFst (a, curs) b = case transform a b of
        Left err -> fail $ "transform failed: " ++ err
        Right (a', _) -> return (a', updateCursor op curs)
