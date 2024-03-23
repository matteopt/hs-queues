module NQs.Common.Types where

import Data.ByteString    ( ByteString )
import Network.Simple.TCP ( Socket, SockAddr )

type Connection = (Socket, SockAddr)
type Message    = ByteString
type QueueName  = ByteString
