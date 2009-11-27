{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module TxPacket where
import Control.Exception
import Data.Binary
import Data.Binary.Put
import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.Socket (HostAddress)
import RawPacket
import Util

data TxPacket
    = AccountLoginFailed
        { reason :: AccountLoginFailReason }
    | ServerList
        { servers :: [ServerListItem] }
    deriving Show
        
data AccountLoginFailReason
    = IncorrectNameOrPassword
    | SomeoneIsAlreadyUsingAccount
    | YourAccountHasBeenBlocked
    | YourAccountCredentialsAreInvalid
    | CommunicationProblem
    deriving Show

data ServerListItem
    = ServerListItem
        { name :: String,
          percentFull :: Int,
          timeZone :: Int,
          hostAddress :: HostAddress
        }
    deriving Show

build :: TxPacket -> RawPacket
-- [0x82] AccountLoginFailed
build AccountLoginFailed{..} =
    RawPacket 0x82 (B.pack [0x82, reasonCode])
    where reasonCode = case reason of
                           IncorrectNameOrPassword           -> 0x00
                           SomeoneIsAlreadyUsingAccount      -> 0x01
                           YourAccountHasBeenBlocked         -> 0x02
                           YourAccountCredentialsAreInvalid  -> 0x03
                           CommunicationProblem              -> 0x04
-- [0xA8] ServerList
build ServerList{..} =
    -- logHex "BUILDING Tx Packet" (lazy2strict raw)
    trace (show raw)
    assert (L.length raw == fromIntegral pLen)   
    RawPacket 0xA8 (lazy2strict raw)
    where numServers = length servers
          pLen = 6 + (numServers * 40)
          rawTop = runPut $ do
              put (0xA8 :: Word8)        -- packet id
              put (fromIntegral pLen :: Word16)       -- packet length
              put (0x00 :: Word8)        -- flags
              put (fromIntegral numServers :: Word16) -- server count
          rawServer (ServerListItem{..}, idx) = runPut $ do
              put (idx :: Word16)                -- server index
              mapM put (truncString name 32) -- name of server
              put (fromIntegral percentFull :: Word8)         -- percentage full
              put (fromIntegral timeZone :: Word8)            -- timezone
              put (hostAddress :: Word32) -- host address
          rawServers = map rawServer (zip servers [0..])
         -- raw = rawTop -- `L.append` L.concat rawServers
          raw = L.concat (rawTop : rawServers)
