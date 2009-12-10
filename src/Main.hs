{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import BoneDewd.Network
import qualified BoneDewd.RxPacket as Rx
import qualified BoneDewd.TxPacket as Tx
import BoneDewd.Types
import BoneDewd.Util
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

loginServer = 
    bracket
        (listenOn (PortNumber 2593))
        sClose
        (forever . loginAccept)

gameServer = 
    bracket
        (listenOn (PortNumber 3593))
        sClose
        (forever . gameAccept)

loginAccept :: Socket -> IO ()
loginAccept service = accept service >>= \(peer,_,_) -> forkIO (handlePeer peer) >> return ()

gameAccept :: Socket -> IO ()
gameAccept service = accept service >>= \(peer,_,_) -> forkIO (handlePeer' peer) >> return ()

-- for login server
handlePeer :: Handle -> IO ()
handlePeer peer = do
    hSetBuffering peer (BlockBuffering (Just 4096))
    work `finally` hClose peer
    where work = do
              let loop = do
                      res <- recvPacket AccountLoginState peer
                      case res of
                          Just rx -> handleRx peer rx >> loop
                          Nothing -> infoM "LoginServer" "PEER CONNECTION CLOSED"
              loop       

-- for game server
handlePeer' :: Handle -> IO ()
handlePeer' peer = do
    hSetBuffering peer (BlockBuffering (Just 4096))
    work `finally` hClose peer
    where work = do
              recvPacket PreGameLoginState peer -- after first packet we are no longer pre-game
              let loop = do
                      res <- recvPacket GameLoginState peer
                      case res of
                          Just rx -> handleRx peer rx >> loop
                          Nothing -> infoM "GameServer" "PEER CONNECTION CLOSED"
              loop                          

handleRx :: Handle -> Rx.RxPacket -> IO ()
handleRx peer Rx.AccountLoginRequest{..} = do
    sendPacket AccountLoginState peer serverList
    --sendPacket peer (Tx.build (Tx.AccountLoginFailed Tx.CommunicationProblem))
handleRx peer Rx.ServerSelect{..} = do
    sendPacket AccountLoginState peer (Tx.ServerRedirect localhost 3593 0)
handleRx peer Rx.GameLoginRequest{..} = do
    sendPacket GameLoginState peer (Tx.CharacterList chars cities)
    where chars = [Tx.CharacterListItem "Fatty Bobo" ""]
          cities = [Tx.StartingCity "Britain" "Da Ghetto"]
handleRx peer Rx.CharacterLoginRequest{..} = do
    sendPacket GameLoginState peer (Tx.LoginConfirm me 6144 4096)
    -- sendPacket GameLoginState peer (Tx.DrawPlayer me)
    -- sendPacket GameLoginState peer (Tx.DrawPlayer me)
handleRx peer (Rx.ClientLanguage _) = do
    sendPacket GameLoginState peer Tx.LoginComplete
    sendPacket GameLoginState peer (Tx.DrawPlayer me)
    --sendPacket GameLoginState peer (Tx.DrawMobile me)   
    sendPacket GameLoginState peer (Tx.StatusBarInfo (Serial 12345) "Fatty Bobo" meStats 0 False)
handleRx peer (Rx.Ping seqid) = do
    sendPacket GameLoginState peer (Tx.Pong seqid)
handleRx peer (Rx.MoveRequest _ s _) = do
    sendPacket InGameState peer (Tx.MoveAccept s Innocent)
handleRx peer Rx.PaperDollRequest = do
    sendPacket InGameState peer (Tx.OpenPaperDoll mySerial "Fatty Bobo the Deusche" myStatus)
handleRx peer (Rx.RequestWarMode wm) = do
    sendPacket InGameState peer (Tx.SetWarMode wm)
handleRx peer (Rx.RequestStatus _) = sendPacket GameLoginState peer meStatusBar
handleRx _ _ = return ()

me :: Mobile
me =
    Mobile mySerial 0x192 255 britainLoc (MobDirection DirDown Running) myStatus Innocent []
    where britainLoc = Loc 1477 1638 50

myStatus = (MobStatus True False False False)
mySerial = Serial 12345

meStatusBar = Tx.StatusBarInfo (Serial 12345) "Fatty Bobo" meStats 0 False

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

serverList :: Tx.TxPacket
serverList =
    Tx.ServerList [Tx.ServerListItem "Test Server" 50 8 localhost]

localhost :: HostAddress
localhost = unsafePerformIO (inet_addr "127.0.0.1")

setupLogging :: IO ()
setupLogging = do
    std <- verboseStreamHandler stdout INFO
    updateGlobalLogger rootLoggerName (System.Log.Logger.setLevel DEBUG)
    -- updateGlobalLogger "RxPacket" (setHandlers [std] . System.Log.Logger.setLevel DEBUG)
    -- updateGlobalLogger "TxPacket" (setHandlers [std] . System.Log.Logger.setLevel DEBUG)
    updateGlobalLogger rootLoggerName (setHandlers [std])
