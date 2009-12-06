module BoneDewd.Network where
import BoneDewd.RawPacket
import BoneDewd.RxPacket
import BoneDewd.TxPacket
import BoneDewd.Types
import Control.Applicative ((<$>))
import Network (Socket)
import System.IO
import System.Log.Logger

recvPacket :: SessionState -> Handle -> IO (Maybe RxPacket)
recvPacket state peer = do
    mraw <- recvRawPacket state peer
    case mraw of
        Nothing -> return Nothing
        Just raw -> do
            case parse state raw of
                Right IgnoredPacket -> return (Just IgnoredPacket)
                Right rx -> do
                    infoM "RxPacket" (show rx)
                    return (Just rx)
                Left err -> do
                    errorM "RxPacket" err
                    return Nothing

sendPacket :: SessionState -> Handle -> TxPacket -> IO ()
sendPacket state peer tx = do
    sendRawPacket state peer (build tx)
    infoM "TxPacket" (show tx)
    return ()
