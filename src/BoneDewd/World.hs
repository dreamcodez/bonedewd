{-# LANGUAGE NamedFieldPuns #-}
module BoneDewd.World (startLoginManager, startGameManager) where
import BoneDewd.Client
import BoneDewd.RxPacket
import BoneDewd.TxPacket
import BoneDewd.Types
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)
import Control.Monad.State
import qualified Data.Map as M
import Network.Socket (HostAddress, inet_addr)
import System.IO.Unsafe
import System.Log.Logger



-- TODO: add callback parameter which authorizes logins
startLoginManager :: IO (Chan (Client,RxPacket))
startLoginManager = do
    ch <- newChan
    let loop = forever $ do
            (c,rx) <- readChan ch
            handleLoginRx rx c
    forkIO loop
    return ch

-- start the manager loop and return the world channel
-- the semantics of handler are that it should do the minimum amount of processing
-- necessary to maintain atomicity and where possible offload long-running
-- work to other threads
startGameManager :: WorldState -> IO (Chan (Client,RxPacket))
startGameManager initState = do
    ch <- newChan
    let loop st = do
            (c,rx) <- readChan ch
            newSt <- execStateT (handleGameRx rx c) st
            loop newSt
    forkIO (loop initState)
    return ch

handleLoginRx :: RxPacket -> Client -> IO ()
handleLoginRx AccountLoginRequest{} Client{clientWrite} = do
    --clientWrite (AccountLoginFailed CommunicationProblem)
    clientWrite serverList
handleLoginRx ServerSelect{} Client{clientWrite,clientDisconnect} = do
    clientWrite (ServerRedirect localhost 3593 0)
    --clientDisconnect
handleLoginRx _ _ = return ()

handleGameRx :: RxPacket -> Client -> StateT WorldState IO ()
handleGameRx CharacterLoginRequest{} (c @ Client{clientId, clientWrite}) =
    do initPlayer (Player c me meStats)
       lift $ clientWrite (CharacterLoginConfirm me 6144 4096)
       lift $ clientWrite CharacterLoginComplete
       lift $ clientWrite (DrawMobile me)
       lift $ clientWrite (DrawMobile other)
handleGameRx GameLoginRequest{} Client{clientWrite} =
    do lift $ clientWrite (CharacterList chars cities)
handleGameRx (Ping seqid) Client{clientWrite} =
    do lift $ clientWrite (Pong seqid)
handleGameRx (MoveRequest _ s _) Client{clientWrite} =
    do lift $ clientWrite (MoveAccept s Innocent)
handleGameRx PaperDollRequest Client{clientWrite} =
    do lift $ clientWrite (OpenPaperDoll mySerial "Fatty Bobo the Deusche" myStatus)
handleGameRx (RequestWarMode wm) Client{clientWrite} =
    do lift $ clientWrite (SetWarMode wm)
handleGameRx (RequestStatus _) Client{clientWrite} =
    do lift $ clientWrite meStatusBar
handleGameRx (SpeechRequest t h f l txt) Client{clientWrite} =
    do lift $ clientWrite (SendUnicodeSpeech mySerial t h f l myName txt)
handleGameRx _ _ = return ()
{-
handleRx :: Handle -> RxPacket -> IO ())
handleRx peer CharacterLoginRequest{..} = do
    sendPacket Compressed peer (CharacterLoginConfirm me 6144 4096)
    -- sendPacket GameLoginState peer (DrawPlayer me)
    -- sendPacket GameLoginState peer (DrawPlayer me)
    sendPacket Compressed peer CharacterLoginComplete
    --sendPacket Compressed peer (SupportedFeatures 0x80FF)
    --sendPacket Compressed peer (DrawPlayer me)
    --sendPacket Compressed peer (SetSeason Summer True)
    sendPacket Compressed peer (DrawMobile me) 
    --sendPacket Compressed peer (SetLightLevel 0x00)  
    --sendPacket Compressed peer (DrawContainer (Serial 54321) (Gump 0x3c)) -- send backpack   
    --sendPacket Compressed peer (StatusBarInfo (Serial 12345) "Fatty Bobo" meStats 0 False)
-}
serverList :: TxPacket
serverList =
    ServerList [ServerListItem "Test Server" 50 8 localhost]

localhost :: HostAddress
--localhost = unsafePerformIO (inet_addr "127.0.0.1")
localhost = unsafePerformIO (inet_addr "10.0.1.7")

chars = [CharacterListItem "Fatty Bobo" ""]
cities = [StartingCity "Britain" "Da Ghetto"]
          
me :: Mobile
me =
    Mobile mySerial 0x190 1002 britainLoc (MobDirection DirDown Running) myStatus Innocent [MobEquipmentItem (Serial 99999) 0x204F 0x16 137]
    where britainLoc = Loc 1477 1638 25

other :: Mobile
other =
    Mobile otherSerial 0x190 1002 britainLoc (MobDirection DirDown Running) myStatus Innocent [MobEquipmentItem (Serial 99998) 0x204F 0x16 137]
    where britainLoc = Loc 1478 1638 50


myStatus = (MobStatus True False False False)
mySerial = Serial 123456
otherSerial = Serial 123457

meStatusBar = StatusBarInfo mySerial myName meStats 0 False
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

-- initialize a new player, check to see if one already exists...
initPlayer :: Player -> StateT WorldState IO ()
initPlayer (p @ Player{playerClient}) = do
    modify (\(s @ WorldState{worldPlayers}) -> do
                let worldPlayers' = M.insert (clientId playerClient) p worldPlayers
                if M.member (clientId playerClient) worldPlayers
                    then error ("initPlayer: player is already active:\n" ++ show p)
                    else s{worldPlayers=worldPlayers'})
    lift $ noticeM "World" ("New player: " ++ show p)

--processMove :: MobDirectionClientId -> StateT WorldState IO ()