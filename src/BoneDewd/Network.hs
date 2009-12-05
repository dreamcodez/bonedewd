module BoneDewd.Network where
import BoneDewd.RawPacket
import BoneDewd.RxPacket
import BoneDewd.TxPacket
import BoneDewd.Types
import Control.Applicative ((<$>))
import Network (Socket)
import System.Log.Logger

recvPacket :: SessionState -> Socket -> IO (Maybe RxPacket)
recvPacket state peer = do
    res <- parse state <$> recvRawPacket state peer
    case res of
        Right rx -> do
            debugM "RxPacket" ("\n" ++ show rx)
            return (Just rx)
        Left err -> do
            debugM "RxPacket" err
            return Nothing

sendPacket :: SessionState -> Socket -> TxPacket -> IO ()
sendPacket state peer tx = do
    sendRawPacket state peer (build tx)
    debugM "TxPacket" ("\n" ++ show tx)
    return ()
