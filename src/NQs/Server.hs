{-# LANGUAGE OverloadedStrings #-}

module NQs.Server where

import Control.Concurrent.MVar
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Network.ByteOrder
import Network.Simple.TCP
import NQs.Common.Types
import NQs.Server.Operations
import NQs.Server.Parse
import NQs.Server.Types

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Map.Strict            as Map

(+++) :: BL.ByteString -> BL.ByteString -> BL.ByteString
(+++) = BL.append

reply :: Connection -> Maybe Message -> IO ()
reply (sock, _) (Just message) = sendLazy sock $
    BL.singleton 3 +++
    (BL.fromStrict $ bytestring32 $ fromIntegral $ B.length message) +++
    BL.fromStrict message
reply (sock, _) Nothing = sendLazy sock $ BL.singleton 4

handle :: Connection -> Maybe OperationResult -> IO ()
handle c (Just (PopResult x)) = reply c x
handle _ Nothing              = pure ()

parse :: Connection -> AP.Result Operation -> MaybeT IO (Operation, B.ByteString)
parse _             (AP.Done    i r  ) = pure (r, i)
parse _             (AP.Fail    _ _ e) = error e
parse c@(sock, _) p@(AP.Partial _    ) =
    parse c =<< AP.feed p <$> (hoistMaybe =<< recv sock 4096)

loop' :: State -> Connection -> B.ByteString -> MaybeT IO ()
loop' state c buf = do
    (op, y) <- parse c $ AP.parse operation buf
    lift (handle c =<< execute state op)
    loop' state c y

loop :: State -> Connection -> MaybeT IO ()
loop state c = loop' state c ""

emptyState :: IO State
emptyState = do
    qs <- newMVar $ Map.empty
    pure $ State { queues = qs }

runServer :: HostName -> IO ()
runServer h = do
    state <- emptyState
    serve (Host h) "7890" $ \c -> runMaybeT (loop state c) >> pure ()
