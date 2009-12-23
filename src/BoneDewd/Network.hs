module BoneDewd.Network where
import BoneDewd.RawPacket
import BoneDewd.RxPacket
import BoneDewd.TxPacket
import BoneDewd.Types
import Control.Applicative ((<$>))
import Network (Socket)
import System.IO
import System.Log.Logger

recvPacket :: ParseState -> Handle -> IO (Maybe (ParseState,RxPacket))
recvPacket state peer = do
    mraw <- recvRawPacket state peer
    case mraw of
        Nothing -> return Nothing
        Just raw -> do
            let pktres = parse state raw
            case pktres of
                Right (newstate,IgnoredPacket) -> return (Just (newstate,IgnoredPacket))
                Right (newstate,rx) -> do
                    infoM "RxPacket" (show rx)
                    return (Just (newstate,rx))
                Left err -> do
                    errorM "RxPacket" err
                    return Nothing

sendPacket :: PacketEncoding -> Handle -> TxPacket -> IO ()
sendPacket enc peer tx = do
    sendRawPacket enc peer (build tx)
    infoM "TxPacket" (show tx)
    return ()
