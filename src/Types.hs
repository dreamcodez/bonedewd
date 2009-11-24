module Types where
import Data.ByteString

data Packet =
    Packet {
      pktId   :: Int,
      pktSize :: Int,
      pktRaw  :: ByteString
    }
