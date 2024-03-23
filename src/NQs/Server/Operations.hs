{-# LANGUAGE OverloadedRecordDot #-}

module NQs.Server.Operations where

import Control.Concurrent.MVar ( modifyMVar, modifyMVar_ )
import Data.Map.Strict         ((!?))
import Data.Sequence           ( (|>), Seq(Empty, (:<|)) )
import NQs.Common.Types
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

pop :: Queue -> (Queue, Maybe Message)
pop (x :<| xs) = (xs   , Just x)
pop Empty      = (Empty, Nothing)

popFrom :: QueueName -> Queues -> (Queues, Maybe Message)
popFrom name qs = case (qs !? name) of
    Just q  -> let (xs, x) = pop q in (Map.insert name xs qs, x      )
    Nothing ->                        (qs                   , Nothing)

popFromMVar :: State -> QueueName -> IO (Maybe Message)
popFromMVar state name = modifyMVar state.queues (pure . popFrom name)

execute :: State -> Operation -> IO (Maybe OperationResult)
execute state (Push name message) = pushToMVar state name message >> pure Nothing
execute state (Pop name) = Just . PopResult <$> popFromMVar state name
