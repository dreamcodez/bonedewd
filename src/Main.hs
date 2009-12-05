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
import Network (PortID(..), listenOn)
import Network.Socket (HostAddress, Socket, accept, sClose, inet_addr)
import System.IO
import System.IO.Unsafe
import System.Log.Logger 
import System.Log.Handler
import System.Log.Handler.Simple

main :: IO ()
main = do
    setupLogging
    forkIO $ loginServer
    gameServer


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
loginAccept service = accept service >>= \(peer,_) -> forkIO (handlePeer peer) >> return ()

gameAccept :: Socket -> IO ()
gameAccept service = accept service >>= \(peer,_) -> forkIO (handlePeer' peer) >> return ()

handlePeer :: Socket -> IO ()
handlePeer peer =
    work `finally` sClose peer
    where work = do
              forever $ do res <- recvPacket AccountLoginState peer
                           case res of
                               Just rx -> handleRx peer rx
                               Nothing -> return ()

-- for game server
handlePeer' :: Socket -> IO ()
handlePeer' peer =
    work `finally` sClose peer
    where work = do
              recvPacket PreGameLoginState peer -- after first packet we are no longer pre-game
              forever $ do res <- recvPacket GameLoginState peer
                           case res of
                               Just rx -> handleRx peer rx
                               Nothing -> return ()                            

handleRx :: Socket -> Rx.RxPacket -> IO ()
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
    updateGlobalLogger rootLoggerName (System.Log.Logger.setLevel DEBUG)
    std <- verboseStreamHandler stdout DEBUG
    --updateGlobalLogger "RxPacket" (setHandlers [std])
    --updateGlobalLogger "TxPacket" (setHandlers [std])
    updateGlobalLogger rootLoggerName (setHandlers [std])
