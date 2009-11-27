{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module Server where
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad
import Network (PortID(..), listenOn)
import Network.Socket (Socket, accept, sClose, inet_addr)
import RawPacket
import Util
import qualified TxPacket as Tx
import qualified RxPacket as Rx
import System.IO
import System.IO.Unsafe
import System.Log.Logger 
import System.Log.Handler
import System.Log.Handler.Simple

main :: IO ()
main = do
    setupLogging
    bracket
        (listenOn (PortNumber 2593))
        sClose
        (forever . accept')

accept' :: Socket -> IO ()
accept' service = accept service >>= \(peer,_) -> forkIO (handlePeer peer) >> return ()

handlePeer :: Socket -> IO ()
handlePeer peer =
    work `finally` sClose peer
    where work = do
              recvExactly peer 4 -- throw away first 4 bytes its junk
              forever $ do rx <- Rx.parse <$> recvPacket peer
                           debugM "RxPacket" (show rx)
                           handleRx peer rx            

handleRx :: Socket -> Rx.RxPacket -> IO ()
handleRx peer Rx.AccountLoginRequest{..} = do
    sendPacket peer (Tx.build serverList)
    --sendPacket peer (Tx.build (Tx.AccountLoginFailed Tx.CommunicationProblem))
handleRx _ _ = return ()

serverList :: Tx.TxPacket
serverList =
    Tx.ServerList [Tx.ServerListItem "Test Server" 50 8 localhost]
    where localhost = unsafePerformIO (inet_addr "127.0.0.1")

setupLogging :: IO ()
setupLogging = do
    updateGlobalLogger rootLoggerName (System.Log.Logger.setLevel DEBUG)
    h <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName (setHandlers [h])
