module Server where
import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char
import Network (PortID(..), listenOn)
import Network.Socket (Socket, accept, sClose)
import Network.Socket.ByteString
import Text.Hexdump


main =
    bracket
        (listenOn (PortNumber 2593))
        sClose
        (forever . accept')
		
accept' service = bracket (fst <$> accept service) sClose handlePeer

handlePeer :: Socket -> IO ()
handlePeer peer = do
  recvExactly peer 4 -- throw away first 4 bytes its junk
  dta <- recvExactly peer 62
  logHex "IB: localhost" dta
  logHex "OB: localhost" denied
  2 <- send peer denied
  return ()

fmtHex :: String -> B.ByteString -> String
fmtHex title dta =
  title ++ "\n" ++
  "-----------------------------------------------------------------------------\n" ++
  hexdump 0 (B.unpack dta) ++ "\n"

logHex :: String -> B.ByteString -> IO ()
logHex title dta = putStrLn (fmtHex title dta)

denied = B.pack [chr 0x82, chr 0x4]

recvExactly :: Socket -> Int -> IO B.ByteString
recvExactly peer nbytes
    | nbytes <= 0 = return B.empty
	| otherwise = do res <- recv peer nbytes
	                 if res == B.empty
		               then error "peer closed"
			           else B.append res <$> recvExactly peer (nbytes - B.length res)

