{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module BoneDewd.Client where
import BoneDewd.RawPacket
import qualified BoneDewd.RxPacket as Rx
import qualified BoneDewd.TxPacket as Tx
import BoneDewd.Types
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Network (HostName, PortNumber)
import System.IO (Handle, hIsClosed)
import System.Log.Logger

newtype ClientId = ClientId Integer

data Client
    = Client
        { clientHost :: HostName,
          clientInbox :: Chan Tx.TxPacket,
          clientDisconnect :: IO ()
        }
    deriving Show

data ClientManagerState
    = ClientManagerState
        { mgrClients :: Map ClientId Client,
          mgrNextClientId :: ClientId Integer
        }
    deriving Show
        
initLoginClient :: ClientManagerState -> (Handle,HostName,PortNumber) -> IO ClientManagerState
initLoginClient (ClientManagerState clients next_cid) (peer,host,_) = do

initGameClient :: ClientManagerState -> (Handle,HostName,PortNumber) -> IO ClientManagerState
initGameClient state 

-- async reads from client, sends to an outbox
-- terminates when peer handle is closed, if the client has stopped sending, or if the packet cannot be parsed
clientReader :: ParseState -> Handle -> Chan Rx.RxPacket -> IO ()
clientReader state peer outbox = do
    connClosed <- hIsClosed peer
    if connClosed
        then return () -- client connection closed; terminate
        else do
            mraw <- recvRawPacket state peer
            case mraw of
                Nothing -> return () -- client stopped sending; terminate
                Just raw -> do
                    case Rx.parse state raw of
                        Left err -> do -- parse error; terminate
                            errorM "RxPacket" err
                            return ()
                        Right (newstate,Rx.IgnoredPacket) -> -- ignore packet; continue
                            clientReader newstate peer outbox
                        Right (newstate,pkt) -> do -- put packet in outbox; continue
                            infoM "RxPacket" (show pkt)
                            writeChan outbox pkt
                            clientReader newstate peer outbox

-- async writes to a client, sends items from passed from an inbox
-- terminates when peer handle is closed
clientWriter :: PacketEncoding -> Handle -> Chan Tx.TxPacket -> IO ()
clientWriter enc peer inbox = do
    connClosed <- hIsClosed peer
    if connClosed
        then return () -- client connection closed; terminate
        else do -- take a packet from outbox and send it; continue
            pkt <- readChan inbox
            sendRawPacket enc peer (Tx.build pkt)
            infoM "TxPacket" (show pkt)
            clientWriter enc peer inbox
