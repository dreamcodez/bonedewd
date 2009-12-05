{-# OPTIONS_GHC -Wall #-}
module BoneDewd.RawPacket where
import Control.Applicative ((<$>))
import BoneDewd.Compression
import qualified Data.ByteString as B
-- import Data.Binary
import Data.Binary.Strict.Get
import Data.Word
import Network (Socket)
import Network.Socket.ByteString
import BoneDewd.Types
import BoneDewd.Util
import System.Log.Logger
    
recvRawPacket :: SessionState -> Socket -> IO RawPacket
recvRawPacket PreGameLoginState peer = RawPacket <$> recvExactly peer 4 -- auth id
recvRawPacket _ peer = do
    beg <- recvExactly peer 1
    let (Right pid,_) = runGet getWord8 beg
    end <- recvAppPacket pid peer
    let raw = beg `B.append` end
    debugM "RawPacket" ("RECEIVED\n" ++ fmtHex raw)
    return (RawPacket raw)

recvAppPacket :: Word8 -> Socket -> IO B.ByteString
-- [0x73] 2 bytes long
recvAppPacket 0x73 peer = recvExactly peer 1
-- [0x80] 62 bytes long
recvAppPacket 0x80 peer = recvExactly peer 61
-- [0x91] 65 bytes long
recvAppPacket 0x91 peer = recvExactly peer 64
-- [0xA0] 3 bytes long
recvAppPacket 0xA0 peer = recvExactly peer 2
-- [0xEF] 21 bytes long
recvAppPacket 0xEF peer = recvExactly peer 20
recvAppPacket pid  _ = error ("received unknown app packet " ++ show pid)

sendRawPacket :: SessionState -> Socket -> RawPacket -> IO ()
sendRawPacket AccountLoginState = sendUncompressedRawPacket
sendRawPacket _                 = sendCompressedRawPacket

sendUncompressedRawPacket :: Socket -> RawPacket -> IO ()
sendUncompressedRawPacket peer (RawPacket raw) = do
    sendAll peer raw
    debugM "RawPacket" ("SENT\n" ++ fmtHex raw)

sendCompressedRawPacket :: Socket -> RawPacket -> IO ()
sendCompressedRawPacket peer (RawPacket raw) = do
    sendAll peer (compress raw)
    debugM "RawPacket" ("SENT COMPRESSED\n" ++ fmtHex raw)
