module NQs.Server.Internal where

import Network.ByteOrder
import Network.Simple.TCP
import NQs.Common.Types
import System.Random.Stateful

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL

(+++) :: BL.ByteString -> BL.ByteString -> BL.ByteString
(+++) = BL.append

reply :: Connection -> Maybe Message -> IO ()
reply (sock, _) (Just message) = sendLazy sock $
    BL.singleton 3 +++
    (BL.fromStrict $ bytestring32 $ fromIntegral $ B.length message) +++
    BL.fromStrict message
reply (sock, _) Nothing = sendLazy sock $ BL.singleton 4

randomEl :: [a] -> IO a
randomEl xs = do
    r <- uniformM globalStdGen :: IO Int
    return (xs !! (r `mod` (length xs)))
