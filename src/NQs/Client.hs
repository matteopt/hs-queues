{-# LANGUAGE OverloadedStrings #-}

module NQs.Client where

import Network.ByteOrder
import Network.Simple.TCP
import NQs.Server.Types

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
    size <- recv sock 4
    case size of
        Nothing -> error "eof"
        Just b  -> do
            let w = word32 b
            message <- recv sock $ fromIntegral w
            case message of
                Nothing -> error "eof"
                Just x  -> return x

pop :: Connection -> QueueName -> IO (Maybe Message)
pop c@(sock, _) name = do
    sendLazy sock $
        BL.singleton 1 +++
        (BL.fromStrict $ bytestring32 $ fromIntegral $ B.length name) +++
        BL.fromStrict name
    r <- recv sock 1
    case r of
        Just "\3" -> Just <$> readSized32 c
        Just "\4" -> return Nothing
        Just _   -> error "unexpected reply"
        Nothing  -> error "eof"

acquire :: HostName -> IO Connection
acquire = flip connectSock "7890"

release :: Connection -> IO ()
release = closeSock . fst
