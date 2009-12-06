{-# OPTIONS_GHC -Wall #-}
module BoneDewd.RawPacket where
import Control.Applicative ((<$>))
import BoneDewd.Compression
import Data.ByteString (hGet, hPut)
import qualified Data.ByteString as B
-- import Data.Binary
import Data.Binary.Strict.Get
import Data.Word
import Network (Socket)
import BoneDewd.Types
import BoneDewd.Util
import System.IO
import System.Log.Logger
import Text.Printf
    
-- when Nothing is returned, this means the connection is dead
recvRawPacket :: SessionState -> Handle -> IO (Maybe RawPacket)
recvRawPacket PreGameLoginState peer = Just . RawPacket <$> hGet peer 4 -- auth id
recvRawPacket _ peer = do
    beg <- hGet peer 1
    if beg == B.empty
        -- half closed state, if this triggers then we probably ended the connection uncleanly
        then return Nothing
        -- continue processing packet
        else do
            let (Right pid,_) = runGet getWord8 beg
            end <- recvAppPacket pid peer
            let raw = beg `B.append` end
            debugM "RawPacket" ("RECEIVED\n" ++ fmtHex raw)
            return $ Just (RawPacket raw)

recvAppPacket :: Word8 -> Handle -> IO B.ByteString
-- [0x00] 104 bytes long
recvAppPacket 0x00 peer = hGet peer 103
-- [0x02] 7 bytes long
recvAppPacket 0x02 peer = hGet peer 6
-- [0x06] 5 bytes long
recvAppPacket 0x06 peer = hGet peer 4
-- [0x09] 5 bytes long
recvAppPacket 0x09 peer = hGet peer 4
-- [0x2C] 2 bytes long
recvAppPacket 0x2C peer = hGet peer 1
-- [0x34] 10 bytes long
recvAppPacket 0x34 peer = hGet peer 9
-- [0x5D] 73 bytes long
recvAppPacket 0x5D peer = hGet peer 72
-- [0x72] 5 bytes long
recvAppPacket 0x72 peer = hGet peer 4
-- [0x73] 2 bytes long
recvAppPacket 0x73 peer = hGet peer 1
-- [0x80] 62 bytes long
recvAppPacket 0x80 peer = hGet peer 61
-- [0x91] 65 bytes long
recvAppPacket 0x91 peer = hGet peer 64
-- [0xA0] 3 bytes long
recvAppPacket 0xA0 peer = hGet peer 2
-- [0xBD] dynamic length
recvAppPacket 0xBD peer = do
    beg <- hGet peer 2
    let (Right plen,_) = runGet getWord16be beg
    end <- hGet peer (fromIntegral $ plen - 3)
    return (beg `B.append` end)
-- [0xBF] dynamic length
recvAppPacket 0xBF peer = do
    beg <- hGet peer 2
    let (Right plen,_) = runGet getWord16be beg
    end <- hGet peer (fromIntegral $ plen - 3)
    return (beg `B.append` end)
-- [0xD9] 199 bytes long
recvAppPacket 0xD9 peer = hGet peer 198
-- [0xEF] 21 bytes long
recvAppPacket 0xEF peer = hGet peer 20
recvAppPacket pid  _ = error ("received unknown app packet " ++ printf "0x%02x" pid)

sendRawPacket :: SessionState -> Handle -> RawPacket -> IO ()
sendRawPacket AccountLoginState = sendUncompressedRawPacket
sendRawPacket _                 = sendCompressedRawPacket

sendUncompressedRawPacket :: Handle -> RawPacket -> IO ()
sendUncompressedRawPacket peer (RawPacket raw) = do
    hPut peer raw
    hFlush peer
    debugM "RawPacket" ("SENT\n" ++ fmtHex raw)

sendCompressedRawPacket :: Handle -> RawPacket -> IO ()
sendCompressedRawPacket peer (RawPacket raw) = do
    hPut peer (compress raw)
    hFlush peer
    debugM "RawPacket" ("SENT COMPRESSED\n" ++ fmtHex raw)
