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
    -- sendPacket GameLoginState peer (Tx.DrawPlayer me)
    sendPacket GameLoginState peer (Tx.DrawPlayer me)
    sendPacket GameLoginState peer (Tx.DrawMobile me)
    sendPacket GameLoginState peer (Tx.DrawMobile me)
    sendPacket GameLoginState peer Tx.LoginComplete
handleRx peer (Rx.Ping seqid) = do
    sendPacket GameLoginState peer (Tx.Pong seqid)
handleRx peer (Rx.MoveRequest _ s _) = do
    sendPacket InGameState peer (Tx.MoveAccept s Innocent)
handleRx _ _ = return ()
me :: Mobile
me =
    Mobile 12345 0x192 255 britainLoc (MobDirection DirDown Running) (MobStatus True False False False) Innocent []
    where britainLoc = Loc 1477 1638 50

serverList :: Tx.TxPacket
serverList =
    Tx.ServerList [Tx.ServerListItem "Test Server" 50 8 localhost]

localhost :: HostAddress
localhost = unsafePerformIO (inet_addr "127.0.0.1")

setupLogging :: IO ()
setupLogging = do
    std <- verboseStreamHandler stdout INFO
    updateGlobalLogger rootLoggerName (System.Log.Logger.setLevel INFO)
    -- updateGlobalLogger "RxPacket" (setHandlers [std] . System.Log.Logger.setLevel DEBUG)
    -- updateGlobalLogger "TxPacket" (setHandlers [std] . System.Log.Logger.setLevel DEBUG)
    updateGlobalLogger rootLoggerName (setHandlers [std])
