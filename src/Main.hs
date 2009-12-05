{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import BoneDewd.Network
import qualified BoneDewd.RxPacket as Rx
import qualified BoneDewd.TxPacket as Tx
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
    forkIO loginServer
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
              forever $ do rx <- recvPacket peer
                           handleRx peer rx    

handlePeer' :: Socket -> IO ()
handlePeer' peer =
    work `finally` sClose peer
    where work = do
              forever $ do rx <- recvPacket peer
                           handleRx peer rx                              

handleRx :: Socket -> Rx.RxPacket -> IO ()
handleRx peer Rx.AccountLoginRequest{..} = do
    sendPacket peer serverList
    --sendPacket peer (Tx.build (Tx.AccountLoginFailed Tx.CommunicationProblem))
handleRx peer Rx.ServerSelect{..} = do
    sendPacket peer (Tx.ServerRedirect localhost 3593 0)
handleRx _ _ = return ()

serverList :: Tx.TxPacket
serverList =
    Tx.ServerList [Tx.ServerListItem "Test Server" 50 8 localhost]

localhost :: HostAddress
localhost = unsafePerformIO (inet_addr "127.0.0.1")

setupLogging :: IO ()
setupLogging = do
    updateGlobalLogger rootLoggerName (System.Log.Logger.setLevel DEBUG)
    h <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName (setHandlers [h])
