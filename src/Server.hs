module Server where
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import qualified Network
import Network.Socket
import Text.Hexdump


main =
    bracket
        (Network.listenOn (Network.PortNumber 2593))
        sClose
        (forever . accept')
		
accept' service = bracket (fst <$> accept service) sClose handlePeer

handlePeer :: Socket -> IO ()
handlePeer peer = do
  (_,4) <- recvLen peer 4 -- throw away first 4 bytes its junk
  (dta,62) <- recvLen peer 62
  logHex "IB: localhost" dta
  logHex "OB: localhost" denied
  2 <- send peer denied
  return ()

fmtHex :: String -> String -> String
fmtHex title dta =
  title ++ "\n" ++
  "-----------------------------------------------------------------------------\n" ++
  (hexdump 0 dta) ++ "\n"

logHex :: String -> String -> IO ()
logHex title dta = putStrLn (fmtHex title dta)

denied = [chr 0x82, chr 0x4]

