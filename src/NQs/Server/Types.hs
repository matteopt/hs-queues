module NQs.Server.Types where

import Control.Concurrent.MVar ( MVar )
import Data.Map.Strict         ( Map )
import Data.Sequence           ( Seq )
import NQs.Common.Types        ( Connection, Message, QueueName )

type Queue  = Seq Message
type Queues = Map QueueName Queue
type Sub    = Connection
type Subs   = Map QueueName [Sub]

data State = State
    { queues :: MVar Queues
    , subs   :: MVar Subs }

data Operation
    = Push QueueName Message
    | Pop  QueueName
    | Sub  QueueName
    deriving Show

data OperationResult
    = PopResult (Maybe Message)
    deriving Show
