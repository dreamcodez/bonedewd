{-# LANGUAGE NamedFieldPuns #-}
module BoneDewd.World (startLoginManager, startGameManager) where
import BoneDewd.Client
import qualified BoneDewd.RxPacket as Rx
import qualified BoneDewd.TxPacket as Tx
import BoneDewd.Types
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Network.Socket (HostAddress, inet_addr)
import System.IO.Unsafe


startLoginManager = startManager handleLoginRx
startGameManager = startManager handleGameRx

-- start the manager loop and return the world channel
-- the semantics of handler are that it should do the minimum amount of processing
-- necessary to maintain atomicity and where possible offload long-running
-- work to other threads
startManager :: (Rx.RxPacket -> Client -> IO ()) -> IO (Chan (Client,Rx.RxPacket))
startManager handler = do
    loginChannel <- newChan
    -- TODO: should signal some sort of cleanup on exit
    forkIO (work loginChannel)
    return loginChannel
    where work ch = do
              (c,rx) <- readChan ch
              handler rx c
              work ch

handleLoginRx :: Rx.RxPacket -> Client -> IO ()
handleLoginRx Rx.AccountLoginRequest{} Client{clientWrite} = do
    --clientWrite (Tx.AccountLoginFailed Tx.CommunicationProblem)
    clientWrite serverList
handleLoginRx Rx.ServerSelect{} Client{clientWrite,clientDisconnect} = do
    clientWrite (Tx.ServerRedirect localhost 3593 0)
    --clientDisconnect
handleLoginRx _ _ = return ()

handleGameRx :: Rx.RxPacket -> Client -> IO ()
handleGameRx Rx.CharacterLoginRequest{} Client{clientWrite} =
    do clientWrite (Tx.CharacterLoginConfirm me 6144 4096)
       clientWrite Tx.CharacterLoginComplete
       clientWrite (Tx.DrawMobile me) 
handleGameRx Rx.GameLoginRequest{} Client{clientWrite} =
    do clientWrite (Tx.CharacterList chars cities)
handleGameRx (Rx.Ping seqid) Client{clientWrite} =
    do clientWrite (Tx.Pong seqid)
handleGameRx (Rx.MoveRequest _ s _) Client{clientWrite} =
    do clientWrite (Tx.MoveAccept s Innocent)
handleGameRx Rx.PaperDollRequest Client{clientWrite} =
    do clientWrite (Tx.OpenPaperDoll mySerial "Fatty Bobo the Deusche" myStatus)
handleGameRx (Rx.RequestWarMode wm) Client{clientWrite} =
    do clientWrite (Tx.SetWarMode wm)
handleGameRx (Rx.RequestStatus _) Client{clientWrite} =
    do clientWrite meStatusBar
handleGameRx (Rx.SpeechRequest t h f l txt) Client{clientWrite} =
    do clientWrite (Tx.SendUnicodeSpeech mySerial t h f l myName txt)
handleGameRx _ _ = return ()
{-
handleRx :: Handle -> Rx.RxPacket -> IO ())
handleRx peer Rx.CharacterLoginRequest{..} = do
    sendPacket Compressed peer (Tx.CharacterLoginConfirm me 6144 4096)
    -- sendPacket GameLoginState peer (Tx.DrawPlayer me)
    -- sendPacket GameLoginState peer (Tx.DrawPlayer me)
    sendPacket Compressed peer Tx.CharacterLoginComplete
    --sendPacket Compressed peer (Tx.SupportedFeatures 0x80FF)
    --sendPacket Compressed peer (Tx.DrawPlayer me)
    --sendPacket Compressed peer (Tx.SetSeason Summer True)
    sendPacket Compressed peer (Tx.DrawMobile me) 
    --sendPacket Compressed peer (Tx.SetLightLevel 0x00)  
    --sendPacket Compressed peer (Tx.DrawContainer (Serial 54321) (Gump 0x3c)) -- send backpack   
    --sendPacket Compressed peer (Tx.StatusBarInfo (Serial 12345) "Fatty Bobo" meStats 0 False)
-}
serverList :: Tx.TxPacket
serverList =
    Tx.ServerList [Tx.ServerListItem "Test Server" 50 8 localhost]

localhost :: HostAddress
--localhost = unsafePerformIO (inet_addr "127.0.0.1")
localhost = unsafePerformIO (inet_addr "10.0.1.7")

chars = [Tx.CharacterListItem "Fatty Bobo" ""]
cities = [Tx.StartingCity "Britain" "Da Ghetto"]
          
me :: Mobile
me =
    Mobile mySerial 0x190 1002 britainLoc (MobDirection DirDown Running) myStatus Innocent [MobEquipmentItem (Serial 99999) 0x204F 0x16 137]
    where britainLoc = Loc 1477 1638 50

myPlayer = Player me meStats
myStatus = (MobStatus True False False False)
mySerial = Serial 123456


meStatusBar = Tx.StatusBarInfo mySerial myName meStats 0 False
myName = "Fatty Bobo"

meStats :: MobileStats
meStats = MobileStats
    { statStr = 100,
      statDex = 25,
      statInt = 100,
      statCap = 225,
      statLuck = 777,
      statCurHits = 85,
      statMaxHits = 100,
      statCurMana = 43,
      statMaxMana = 100,
      statCurStam = 39,
      statMaxStam = 50,
      statCurWeight = 333,
      statMaxWeight = 500,
      statCurFollow = 0,
      statMaxFollow = 5,
      statGold = 133,
      statMinDmg = 10,
      statMaxDmg = 33,
      statResistPhysical = 100,
      statResistFire = 100,
      statResistCold = 13,
      statResistPoison = 44,
      statResistEnergy = 32
    }
