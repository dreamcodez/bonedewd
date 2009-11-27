{-# OPTIONS_GHC -Wall #-}
module Server where
import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad
import Network (PortID(..), listenOn)
import Network.Socket (Socket, accept, sClose, inet_addr)
import RawPacket
import Util
import qualified TxPacket as Tx

main :: IO ()
main =
    bracket
        (listenOn (PortNumber 2593))
        sClose
        (forever . accept')

accept' :: Socket -> IO ()
accept' service = bracket (fst <$> accept service) sClose handlePeer

handlePeer :: Socket -> IO ()
handlePeer peer = do
  recvExactly peer 4 -- throw away first 4 bytes its junk
  recvPacket peer
  --sendPacket peer (Tx.build (Tx.AccountLoginFailed Tx.CommunicationProblem))
  localhost <- inet_addr "127.0.0.1"
  sendPacket peer (Tx.build (Tx.ServerList [Tx.ServerListItem "Test Server" 50 8 localhost]))
  