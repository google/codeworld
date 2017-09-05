{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}

module Control.OperationalTransformation.ClientServerTests
  ( tests
  ) where

import Control.OperationalTransformation
import Control.OperationalTransformation.Client
import Control.OperationalTransformation.Server
import Data.Maybe (fromJust)

import Control.OperationalTransformation.Text.Gen (genOperation)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Property

type Queue a = [a]

emptyQueue :: Queue a
emptyQueue = []

appendQueue :: a -> Queue a -> Queue a
appendQueue a q = q ++ [a]

type ClientId = Int

data ExtendedClient doc op = ExtendedClient { clientId :: !ClientId
                                            , clientRevision :: !Revision
                                            , clientSendQueue :: Queue (Revision, op)
                                            , clientReceiveQueue :: Queue (Maybe op)
                                            , clientDoc :: !doc
                                            , clientState :: !(ClientState op)
                                            } deriving (Show)

prop_client_server :: (Eq doc, Arbitrary doc, OTSystem doc op, OTComposableOperation op)
                   => (doc -> Gen op) -> Property
prop_client_server genOp = property $ do
  doc <- arbitrary
  let server = initialServerState doc
      clients = createClients doc $ take numClients [1..]
  (server', clients') <- simulate numActions server clients
  return $ if not (all isSynchronized clients')
    then property $ failed { reason = "some clients are not synchronized" }
    else let ServerState _ doc' _ = server'
         in if all ((== doc') . clientDoc) clients'
              then property True
              else property $ failed { reason = "client documents did not converge" }

  where
    numClients, numActions :: Int
    numClients = 2
    numActions = 100

    firstRevision = 0
    createClients doc = map $ \n ->
      ExtendedClient { clientId = n
                     , clientRevision = firstRevision
                     , clientSendQueue = emptyQueue
                     , clientReceiveQueue = emptyQueue
                     , clientDoc = doc
                     , clientState = initialClientState
                     }

    simulate !n !server !clients = do
      clientN <- choose (0, length clients - 1)
      actionN <- choose (0, 2) :: Gen Int
      let client = clients !! clientN
      (server', clients') <- case actionN of
        0 | canReceive client -> do
          let client' = receiveClient client
          return (server, replace clientN client' clients)
        1 | canSend client -> do
          let ((rev, op), client') = sendClient client
              Right (op', (), server') = applyOperation server rev op ()
              clients' = replace clientN client' clients
              clients'' = broadcast (clientId client) op' clients'
          return (server', clients'')
        _ | n < 0 -> return (server, clients)
          | otherwise -> do
          client' <- editClient client
          return (server, replace clientN  client' clients)
      if n > 0 || any (\c -> canReceive c || canSend c) clients'
        then simulate (n-1) server' clients'
        else return (server', clients')

    replace 0 e (_:xs) = e:xs
    replace n e (x:xs) = x:(replace (n-1) e xs)
    replace _ _ [] = error "replacing empty list"

    canReceive = not . null . clientReceiveQueue
    canSend = not . null . clientSendQueue

    receiveClient client = case clientReceiveQueue client of
      [] -> error "empty receive queue"
      msg:ops ->
        let
          client' = client { clientReceiveQueue = ops
                           , clientRevision = clientRevision client + 1
                           }
        in case msg of
          Nothing -> case fromJust $ serverAck (clientState client') of
            (Just op, clientState') -> client'
              { clientState = clientState'
              , clientSendQueue = appendQueue (clientRevision client', op) (clientSendQueue client')
              }
            (Nothing, clientState') -> client' { clientState = clientState' }
          Just op -> case applyServer (clientState client) op of
            Left err -> error $ "should not happen: " ++ err
            Right (op', clientState') -> case apply op' (clientDoc client') of
              Left err ->  error $ "apply failed: " ++ err
              Right doc' -> client' { clientState = clientState', clientDoc = doc' }

    sendClient client = case clientSendQueue client of
      [] -> error "empty send queue"
      op:ops -> (op, client { clientSendQueue = ops })

    editClient client = do
      op <- genOp $ clientDoc client
      let doc' = fromRight $ apply op $ clientDoc client
          (shouldSend, state') = fromRight $ applyClient (clientState client) op
          client' = client { clientState = state', clientDoc = doc' }
      return $ if shouldSend
        then client' { clientSendQueue = appendQueue (clientRevision client', op) (clientSendQueue client) }
        else client'

    fromRight (Right a) = a
    fromRight (Left err) = error err

    broadcast creator op = map $ \client ->
      let msg = if creator == clientId client then Nothing else Just op
      in client { clientReceiveQueue = appendQueue msg (clientReceiveQueue client) }

    isSynchronized client = case clientState client of
      ClientSynchronized -> True
      _ -> False

tests :: Test
tests = testGroup "Control.OperationalTransformation.ClientServerTests"
  [ testProperty "prop_client_server" $ prop_client_server genOperation
  ]