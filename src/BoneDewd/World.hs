{-# LANGUAGE NamedFieldPuns #-}
module BoneDewd.World (startLoginManager, startGameManager) where
import BoneDewd.Client
import BoneDewd.MapParse
import BoneDewd.RxPacket
import BoneDewd.TxPacket
import BoneDewd.Types
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Monad (forever)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import Network.Socket (HostAddress, inet_addr)
import System.IO.Unsafe
import System.Log.Logger



-- TODO: add callback parameter which authorizes logins
startLoginManager :: IO (TChan (Client,RxPacket))
startLoginManager = do
    ch <- atomically newTChan
    let loop = forever $ do
            (c,rx) <- atomically (readTChan ch)
            handleLoginRx rx c
    forkIO loop
    return ch

-- start the manager loop and return the world channel
-- the semantics of handler are that it should do the minimum amount of processing
-- necessary to maintain atomicity and where possible offload long-running
-- work to other threads
startGameManager :: WorldState -> IO (TChan (Client,RxPacket))
startGameManager initState = do
    ch <- atomically newTChan
    let loop st = do
            (c,rx) <- atomically (readTChan ch)
            (_,newSt) <- execStateT (handleGameRx rx) (c,st)
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

handleGameRx :: RxPacket -> StateT (Client,WorldState) IO ()
handleGameRx CharacterLoginRequest{} =
    do c <- getClient
       initPlayer (Player c me meStats)
       write (CharacterLoginConfirm me 6144 4096)
       write CharacterLoginComplete
       write (DrawMobile me)
       write (DrawMobile other)
handleGameRx GameLoginRequest{} =
    do write (CharacterList chars cities)
handleGameRx (Ping seqid) =
    do write (Pong seqid)
handleGameRx (MoveRequest newDir s _) =
    do processMove newDir s
handleGameRx PaperDollRequest =
    do write (OpenPaperDoll mySerial "Fatty Bobo the Deusche" myStatus)
handleGameRx (RequestWarMode wm) =
    do write (SetWarMode wm)
handleGameRx (RequestStatus _) =
    do write meStatusBar
handleGameRx (SpeechRequest t h f l txt) =
    do write (SendUnicodeSpeech mySerial t h f l myName txt)
handleGameRx _ = return ()
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
initPlayer :: Player -> StateT (Client,WorldState) IO ()
initPlayer (p @ Player{playerClient}) = do
    modify (\(c,s @ WorldState{worldPlayers}) -> do
                let worldPlayers' = M.insert (clientId playerClient) p worldPlayers
                if M.member (clientId playerClient) worldPlayers
                    then error ("initPlayer: player is already active:\n" ++ show p)
                    else (c,s{worldPlayers=worldPlayers'}))
    lift $ noticeM "World" ("New player: " ++ show p)

processMove :: MobDirection -> Word8 -> StateT (Client,WorldState) IO ()
processMove (MobDirection newDir walk_or_run) s = do
    player <- fromJust <$> getPlayer
    let (MobDirection prevDir _) = (mobDirection . playerMobile) player
        (Loc prevX prevY _) = (mobLoc . playerMobile) player
        cid = (clientId . playerClient) player
    if newDir == prevDir
        then do -- movement
            let (newX,newY) = case newDir of
                                  DirNorth -> (prevX, prevY-1)
                                  DirRight -> (prevX+1, prevY-1)
                                  DirEast  -> (prevX+1, prevY)
                                  DirDown  -> (prevX+1, prevY+1)
                                  DirSouth -> (prevX, prevY+1)
                                  DirLeft  -> (prevX-1, prevY+1)
                                  DirWest  -> (prevX-1, prevY)
                                  DirUp    -> (prevX-1, prevY-1)
            newZ <- lift $ getElevation (newX,newY)
            let newLoc = Loc newX newY newZ
            (c,world) <- get
            let oldPlayers = worldPlayers world
                oldPlayer = fromJust $ M.lookup cid oldPlayers
                oldMobile = playerMobile oldPlayer
                newMobile = oldMobile{mobLoc=newLoc,mobDirection=MobDirection newDir walk_or_run}
                newPlayer = oldPlayer{playerMobile=newMobile}
                newPlayers = M.insert cid newPlayer oldPlayers
            put (c,world{worldPlayers=newPlayers})
            write (MoveAccept s Innocent)
            lift $ noticeM "World" ("Player Moved: " ++ show newLoc)
        else do -- direction change only
            (c,world) <- get
            let oldPlayers = worldPlayers world
                oldPlayer = fromJust $ M.lookup cid oldPlayers
                oldMobile = playerMobile oldPlayer
                newMobile = oldMobile{mobDirection=MobDirection newDir walk_or_run}
                newPlayer = oldPlayer{playerMobile=newMobile}
                newPlayers = M.insert cid newPlayer oldPlayers
            put (c,world{worldPlayers=newPlayers})
            write (MoveAccept s Innocent)
            lift $ noticeM "World" ("Player Changed Direction: " ++ show newDir)

getClient :: StateT (Client,WorldState) IO Client
getClient = gets fst

getWorld :: StateT (Client,WorldState) IO WorldState
getWorld = gets snd

getPlayer :: StateT (Client,WorldState) IO (Maybe Player)
getPlayer = do
    cid <- clientId <$> getClient
    M.lookup cid . worldPlayers <$> getWorld

write :: TxPacket -> StateT (Client,WorldState) IO ()
write tx = do
    c <- getClient
    lift $ (clientWrite c) tx
