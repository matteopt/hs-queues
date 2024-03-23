module NQs.Common.Internal where

import Control.Monad.IO.Class
import Network.Simple.TCP

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BU
import qualified Data.ByteString.Lazy    as BL

recvAll :: MonadIO m => Socket -> Int -> m BL.ByteString
recvAll = r mempty where
    r buf _    0 = return $ BU.toLazyByteString buf
    r buf sock m = do
        chunk <- recv sock m
        case chunk of
            Nothing -> error "eof"
            Just x -> r (buf <> (BU.byteString x)) sock (m - (B.length x))
