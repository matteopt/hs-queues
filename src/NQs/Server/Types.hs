module NQs.Server.Types where

import Control.Concurrent.MVar ( MVar )
import Data.ByteString    ( ByteString )
import Data.Map.Strict    ( Map )
import Data.Sequence      ( Seq )
import Network.Simple.TCP ( SockAddr, Socket )

type Connection = (Socket, SockAddr)
type Message    = ByteString
type Queue      = Seq Message
type QueueName  = ByteString
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
