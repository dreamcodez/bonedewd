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

-- this is workaround for ctrl-c on windows
waitForTermination = do
    l <- getLine
    case l of
        "q" -> return ()
        _   -> waitForTermination

main :: IO ()
main = do
    setupLogging
    forkIO $ loginServer
    forkIO $ gameServer
    waitForTermination

loginServer = do
    svc <- listenOn (PortNumber 2593)
    loginChan <- startLoginManager
    let state = newClientManagerState loginChan
    loginAcceptLoop state svc `finally` sClose svc

gameServer = do
    svc <- listenOn (PortNumber 3593)
    gameChan <- startGameManager emptyWorldState
    let state = newClientManagerState gameChan
    gameAcceptLoop state svc `finally` sClose svc

loginAcceptLoop :: ClientManagerState -> Socket -> IO ()
loginAcceptLoop state svc = do
    newState <- (accept svc >>= initLoginClient state)
    loginAcceptLoop newState svc

gameAcceptLoop :: ClientManagerState -> Socket -> IO ()
gameAcceptLoop state svc = do
    newState <- (accept svc >>= initGameClient state)
    gameAcceptLoop newState svc

setupLogging :: IO ()
setupLogging = do
    std <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName (System.Log.Logger.setLevel INFO)
    -- updateGlobalLogger "RxPacket" (setHandlers [std] . System.Log.Logger.setLevel DEBUG)
    -- updateGlobalLogger "TxPacket" (setHandlers [std] . System.Log.Logger.setLevel DEBUG)
    updateGlobalLogger rootLoggerName (setHandlers [std])
