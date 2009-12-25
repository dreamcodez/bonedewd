{-# LANGUAGE NamedFieldPuns #-}
module BoneDewd.World where
import BoneDewd.Client
import qualified BoneDewd.RxPacket as Rx
import qualified BoneDewd.TxPacket as Tx
import BoneDewd.Types
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Network.Socket (HostAddress, inet_addr)
import System.IO.Unsafe

-- start the world manager loop and return the world channel (inc)
startLoginManager :: IO (Chan (Client,Rx.RxPacket))
startLoginManager = do
    loginChannel <- newChan
    -- TODO: should signal some sort of cleanup on exit
    forkIO (work loginChannel)
    return loginChannel
    where work ch = do
              (c,rx) <- readChan ch
              handleLoginRx rx c
              work ch

handleLoginRx :: Rx.RxPacket -> Client -> IO ()
handleLoginRx Rx.AccountLoginRequest{} Client{clientWrite} = do
    --clientWrite (Tx.AccountLoginFailed Tx.CommunicationProblem)
    clientWrite serverList
handleLoginRx Rx.ServerSelect{} Client{clientWrite,clientDisconnect} = do
    clientWrite (Tx.ServerRedirect localhost 3593 0)
    --clientDisconnect
handleLoginRx _ _ = return ()

{-
handleRx :: Handle -> Rx.RxPacket -> IO ()
handleRx peer Rx.AccountLoginRequest{..} = do
    sendPacket NotCompressed peer serverList
    --sendPacket peer (Tx.build (Tx.AccountLoginFailed Tx.CommunicationProblem))
handleRx peer Rx.ServerSelect{..} = do
    sendPacket NotCompressed peer (Tx.ServerRedirect localhost 3593 0)
handleRx peer Rx.GameLoginRequest{..} = do
    sendPacket Compressed peer (Tx.CharacterList chars cities)
    where chars = [Tx.CharacterListItem "Fatty Bobo" ""]
          cities = [Tx.StartingCity "Britain" "Da Ghetto"]
handleRx peer Rx.CharacterLoginRequest{..} = do
    sendPacket Compressed peer (Tx.LoginConfirm me 6144 4096)
    -- sendPacket GameLoginState peer (Tx.DrawPlayer me)
    -- sendPacket GameLoginState peer (Tx.DrawPlayer me)
handleRx peer (Rx.ClientLanguage _) = do
    sendPacket Compressed peer Tx.LoginComplete
    sendPacket Compressed peer (Tx.DrawPlayer me)
    --sendPacket GameLoginState peer (Tx.DrawMobile me)   
    sendPacket Compressed peer (Tx.StatusBarInfo (Serial 12345) "Fatty Bobo" meStats 0 False)
handleRx peer (Rx.Ping seqid) = do
    sendPacket Compressed peer (Tx.Pong seqid)
handleRx peer (Rx.MoveRequest _ s _) = do
    sendPacket Compressed peer (Tx.MoveAccept s Innocent)
handleRx peer Rx.PaperDollRequest = do
    sendPacket Compressed peer (Tx.OpenPaperDoll mySerial "Fatty Bobo the Deusche" myStatus)
handleRx peer (Rx.RequestWarMode wm) = do
    sendPacket Compressed peer (Tx.SetWarMode wm)
handleRx peer (Rx.RequestStatus _) = sendPacket Compressed peer meStatusBar
handleRx _ _ = return ()
-}
serverList :: Tx.TxPacket
serverList =
    Tx.ServerList [Tx.ServerListItem "Test Server" 50 8 localhost]

localhost :: HostAddress
--localhost = unsafePerformIO (inet_addr "127.0.0.1")
localhost = unsafePerformIO (inet_addr "10.0.1.7")