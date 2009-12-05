module BoneDewd.Network where
import BoneDewd.RawPacket
import BoneDewd.RxPacket
import BoneDewd.TxPacket
import Control.Applicative ((<$>))
import Network (Socket)
import System.Log.Logger

recvPacket :: Socket -> IO RxPacket
recvPacket peer = do
    rx <- parse <$> recvRawPacket peer
    debugM "RxPacket" (show rx)
    return rx

sendPacket :: Socket -> TxPacket -> IO ()
sendPacket peer pkt = do
    sendRawPacket peer (build pkt)
    debugM "TxPacket" (show pkt)
    return ()
