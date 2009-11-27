{-# OPTIONS_GHC -Wall #-}
module RawPacket where
import Control.Applicative ((<$>))
import Compression
import qualified Data.ByteString as B
import Data.Binary
import Data.Binary.Get
import Data.Int
import Data.Word
import Network (Socket)
import Network.Socket.ByteString
import Util
import System.Log.Logger

data RawPacket =
    RawPacket {
        pktId        :: Word8,
        pktRaw       :: B.ByteString
    }

instance Show RawPacket where
    show p = fmtHex (show (pktId p)) (pktRaw p)

recvPacket :: Socket -> IO RawPacket
recvPacket peer = do
    beg <- recvExactly peer 1
    let pid = runGet (get :: Get Word8) (strict2lazy beg)
    end <- recvPacket' pid peer
    let pkt = RawPacket pid (beg `B.append` end)
    debugM "RawPacket" $ fmtHex "RECEIVED" (pktRaw pkt)
    return pkt

-- recvPacket' handles obtaining the correct packet length
recvPacket' :: Word8 -> Socket -> IO B.ByteString
-- [0x80] 62 bytes long
recvPacket' 0x80 peer = recvExactly peer 61
-- [0x80] 3 bytes long
recvPacket' 0xA0 peer = recvExactly peer 2
recvPacket' pid  _ = error ("unknown packet " ++ show pid)

sendPacket :: Socket -> RawPacket -> IO ()
sendPacket peer pkt = do
    sendAll peer (pktRaw pkt)
    debugM "RawPacket" $ fmtHex "SENT" (pktRaw pkt)

sendCompressedPacket :: Socket -> RawPacket -> IO ()
sendCompressedPacket peer pkt = do
    sendAll peer (compress (pktRaw pkt))
    debugM "RawPacket" $ fmtHex "SENT COMPRESSED" (pktRaw pkt)
    
accountLoginDenied :: RawPacket
accountLoginDenied = RawPacket 0x82 (B.pack [0x82 :: Word8, 0x04 :: Word8])
