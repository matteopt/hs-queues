{-# LANGUAGE OverloadedStrings #-}

module NQs.Client where

import Network.ByteOrder
import Network.Simple.TCP
import NQs.Common.Internal
import NQs.Common.Types

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL

(+++) :: BL.ByteString -> BL.ByteString -> BL.ByteString
(+++) = BL.append

push :: Connection -> QueueName -> Message -> IO ()
push (sock, _) name message = sendLazy sock $
    BL.singleton 0 +++
    (BL.fromStrict $ bytestring32 $ fromIntegral $ B.length name) +++
    BL.fromStrict name +++
    (BL.fromStrict $ bytestring32 $ fromIntegral $ B.length message) +++
    BL.fromStrict message

readSized32 :: Connection -> IO (Message)
readSized32 (sock, _) = do
    size <- recvAll sock 4
    let w = word32 $ B.toStrict size
    message <- recvAll sock $ fromIntegral w
    return $ B.toStrict message

pop :: Connection -> QueueName -> IO (Maybe Message)
pop c@(sock, _) name = do
    sendLazy sock $
        BL.singleton 1 +++
        (BL.fromStrict $ bytestring32 $ fromIntegral $ B.length name) +++
        BL.fromStrict name
    r <- recvAll sock 1
    case r of
        "\3" -> Just <$> readSized32 c
        "\4" -> return Nothing
        x    -> print x >> error "unexpected reply"

acquire :: HostName -> IO Connection
acquire = flip connectSock "7890"

release :: Connection -> IO ()
release = closeSock . fst
