{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module BoneDewd.Client where
import BoneDewd.RawPacket
import qualified BoneDewd.RxPacket as Rx
import qualified BoneDewd.TxPacket as Tx
import BoneDewd.Types
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (finally)
import qualified Data.Map as M
import Network (HostName, PortNumber)
import System.IO (Handle, hClose, hIsClosed)
import System.Log.Logger

newtype ClientId = ClientId Integer deriving (Eq,Num,Ord,Show)

data Client
    = Client
        { clientHost :: HostName,
          clientWrite :: Tx.TxPacket -> IO (),
          clientDisconnect :: IO ()
        }

data ClientManagerState
    = ClientManagerState
        { mgrClients :: M.Map ClientId Client,
          mgrNextClientId :: ClientId,
          mgrWorldChan :: Chan (Client,Rx.RxPacket)
        }
newClientManagerState :: Chan (Client,Rx.RxPacket) -> ClientManagerState
newClientManagerState ch = ClientManagerState M.empty 1 ch

initLoginClient :: ClientManagerState -> (Handle,HostName,PortNumber) -> IO ClientManagerState
initLoginClient (ClientManagerState clients next_cid worldChan) (peer,host,_) = do
    inbox <- newChan
    let newClient = Client host (writeChan inbox) (hClose peer)
        newClients = M.insert next_cid newClient clients
        newState = ClientManagerState newClients (next_cid + 1) worldChan
    forkIO (clientReader LoginState newClient peer worldChan `finally` cleanup)
    forkIO (clientWriter NotCompressed peer inbox `finally` cleanup)
    return newState
    where cleanup = do
              hClose peer
              infoM "LoginServer" "PEER CONNECTION CLOSED"
          

-- initGameClient :: ClientManagerState -> (Handle,HostName,PortNumber) -> IO (ClientManagerState,Client)
-- initGameClient state 

-- async reads from client, sends to an outbox
-- terminates when peer handle is closed, if the client has stopped sending, or if the packet cannot be parsed
clientReader :: ParseState -> Client -> Handle -> Chan (Client,Rx.RxPacket) -> IO ()
clientReader initState client peer outbox =
    work initState
    where work state = do
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
                                      work newstate
                                  Right (newstate,pkt) -> do -- put packet in outbox; continue
                                      infoM "RxPacket" (show pkt)
                                      writeChan outbox (client,pkt)
                                      work newstate

-- async writes to a client, sends items from passed inbox
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
