{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import BoneDewd.Client
import BoneDewd.Network
import qualified BoneDewd.RxPacket as Rx
import qualified BoneDewd.TxPacket as Tx
import BoneDewd.Types
import BoneDewd.Util
import BoneDewd.World
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Word
import Network (PortID(..), accept, listenOn)
import Network.Socket (HostAddress, Socket, inet_addr, sClose)
import System.IO
import System.IO.Unsafe
import System.Log.Logger 
import System.Log.Handler
import System.Log.Handler.Simple

-- only works on windows
waitForTermination = do
    l <- getLine
    case l of
        "quit" -> return ()
        _   -> waitForTermination

main :: IO ()
main = do
    setupLogging
    forkIO $ loginServer
    forkIO $ gameServer
    waitForTermination
{-
loginServer = 
    bracket
        (listenOn (PortNumber 2593))
        sClose
        (forever . loginAccept)
-}

loginServer = do
    svc <- listenOn (PortNumber 2593)
    loginChan <- startLoginManager
    let state = newClientManagerState loginChan
    loginAcceptLoop state svc `finally` sClose svc

gameServer = 
    bracket
        (listenOn (PortNumber 3593))
        sClose
        (forever . gameAccept)

{-
loginAccept :: ClientManagerState -> (Handle,HostName,PortNumber) -> IO ()
loginAccept service = accept service >>= \(peer,_,_) -> forkIO (handlePeer peer) >> return ()
-}

loginAcceptLoop :: ClientManagerState -> Socket -> IO ()
loginAcceptLoop state svc = do
    newState <- (accept svc >>= initLoginClient state)
    loginAcceptLoop newState svc
    

gameAccept :: Socket -> IO ()
gameAccept service = accept service >>= \(peer,_,_) -> forkIO (handlePeer' peer) >> return ()

-- for login server
handlePeer :: Handle -> IO ()
handlePeer peer = do
    hSetBuffering peer (BlockBuffering (Just 4096))
    work `finally` hClose peer
    where work = do
              let loop state = do
                      res <- recvPacket state peer
                      case res of
                          Just (newstate,rx) -> handleRx peer rx >> loop newstate
                          Nothing -> infoM "LoginServer" "PEER CONNECTION CLOSED"
              loop LoginState

-- for game server
handlePeer' :: Handle -> IO ()
handlePeer' peer = do
    hSetBuffering peer (BlockBuffering (Just 4096))
    work `finally` hClose peer
    where work = do
              let loop state = do
                      res <- recvPacket state peer
                      case res of
                          Just (newstate,rx) -> handleRx peer rx >> loop newstate
                          Nothing -> infoM "GameServer" "PEER CONNECTION CLOSED"
              loop PreGameState          

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
--handleRx peer (Rx.ClientLanguage _) = do  
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

me :: Mobile
me =
    Mobile mySerial 0x190 1655 britainLoc (MobDirection DirDown Running) myStatus Innocent []
    where britainLoc = Loc 1477 1638 50

myPlayer = Player me meStats
myStatus = (MobStatus True False False False)
mySerial = Serial 123456


meStatusBar = Tx.StatusBarInfo mySerial "Fatty Bobo" meStats 0 False

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

setupLogging :: IO ()
setupLogging = do
    std <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName (System.Log.Logger.setLevel DEBUG)
    -- updateGlobalLogger "RxPacket" (setHandlers [std] . System.Log.Logger.setLevel DEBUG)
    -- updateGlobalLogger "TxPacket" (setHandlers [std] . System.Log.Logger.setLevel DEBUG)
    updateGlobalLogger rootLoggerName (setHandlers [std])
