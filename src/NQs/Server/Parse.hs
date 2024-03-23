module NQs.Server.Parse where

import Control.Applicative        ( Alternative((<|>)) )
import Data.Attoparsec.Binary     ( anyWord32be )
import Data.Attoparsec.ByteString ( word8, Parser )
import Data.ByteString            ( ByteString )
import NQs.Server.Types           ( Operation(..) )

import qualified Data.Attoparsec.ByteString as AP

sized32 :: Parser ByteString
sized32 = anyWord32be >>= AP.take . fromIntegral

push :: Parser Operation
push = Push <$> (word8 0 *> sized32) <*> sized32

pop :: Parser Operation
pop = Pop <$> (word8 1 *> sized32)

sub :: Parser Operation
sub = Sub <$> (word8 2 *> sized32)

operation :: Parser Operation
operation = push <|> pop <|> sub
