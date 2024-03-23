module NQs.Server.Types where

import Control.Concurrent.MVar ( MVar )
import Data.Map.Strict         ( Map )
import Data.Sequence           ( Seq )
import NQs.Common.Types        ( QueueName, Message )

type Queue      = Seq Message
type Queues     = Map QueueName Queue

data State = State
    { queues :: MVar Queues }

data Operation
    = Push QueueName Message
    | Pop  QueueName
    deriving Show

data OperationResult
    = PopResult (Maybe Message)
    deriving Show
