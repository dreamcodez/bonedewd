module BoneDewd.Network where
import BoneDewd.RawPacket
import BoneDewd.RxPacket
import BoneDewd.TxPacket
import BoneDewd.Types
import Control.Applicative ((<$>))
import Network (Socket)
import System.Log.Logger

recvPacket :: SessionState -> Socket -> IO RxPacket
recvPacket state peer = do
    rx <- parse state <$> recvRawPacket state peer
    debugM "RxPacket" ("\n" ++ show rx)
    return rx

sendPacket :: SessionState -> Socket -> TxPacket -> IO ()
sendPacket state peer tx = do
    sendRawPacket state peer (build tx)
    debugM "TxPacket" ("\n" ++ show tx)
    return ()
