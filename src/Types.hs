module Types where
import Data.ByteString
import Data.Time

data Packet =
    Packet {
      pktId   :: Int,
      pktSize :: Int,
      pktTimestamp :: UTCTime,
      pktRaw  :: ByteString
    }
