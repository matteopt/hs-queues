{-# LANGUAGE OverloadedRecordDot #-}

module NQs.Server.Operations where

import Control.Concurrent.MVar ( readMVar, modifyMVar, modifyMVar_ )
import Data.Map.Strict         ((!?))
import Data.Sequence           ( (|>), Seq(Empty, (:<|)) )
import NQs.Common.Types
import NQs.Server.Internal
import NQs.Server.Types

import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Sequence

push :: Message -> Queue -> Queue
push = flip (|>)

pushTo :: QueueName -> Message -> Queues -> Queues
pushTo name message qs = Map.insert name (push message q) qs where
    q = Map.findWithDefault Sequence.empty name qs

pushToMVar :: State -> QueueName -> Message -> IO ()
pushToMVar state name message = modifyMVar_ state.queues (pure . pushTo name message)

decidePush :: State -> QueueName -> Message -> IO ()
decidePush state name message = do
    ss <- readMVar state.subs
    case (ss !? name) of
        Nothing  -> pushToMVar state name message
        Just []  -> pushToMVar state name message
        Just ss' -> randomEl ss' >>= (flip reply) (Just message)
    return ()

pop :: Queue -> (Queue, Maybe Message)
pop (x :<| xs) = (xs   , Just x)
pop Empty      = (Empty, Nothing)

popFrom :: QueueName -> Queues -> (Queues, Maybe Message)
popFrom name qs = case (qs !? name) of
    Just q  -> let (xs, x) = pop q in (Map.insert name xs qs, x      )
    Nothing ->                        (qs                   , Nothing)

popFromMVar :: State -> QueueName -> IO (Maybe Message)
popFromMVar state name = modifyMVar state.queues (pure . popFrom name)

subTo :: Connection -> QueueName -> Subs -> Subs
subTo c name ss = Map.insert name (c:ss') ss where
    ss' = Map.findWithDefault [] name ss

subToState :: State -> Connection -> QueueName -> IO ()
subToState state c name = modifyMVar_ state.subs (pure . subTo c name)

execute :: State -> Connection -> Operation -> IO (Maybe OperationResult)
execute state _ (Push name message) = decidePush state name message >> pure Nothing
execute state _ (Pop name) = Just . PopResult <$> popFromMVar state name
execute state c (Sub name) = subToState state c name >> pure Nothing
