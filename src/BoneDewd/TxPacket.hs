{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module BoneDewd.TxPacket where
import Control.Exception
import Data.Binary
import Data.Binary.Put
import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.Socket (HostAddress)
import BoneDewd.RawPacket
import BoneDewd.Util

data TxPacket
    = ServerList
        { servers :: [ServerListItem] }
    | ServerRedirect
        { redirectHostAddress :: HostAddress,
          redirectHostPort :: Int,
          encryptionKey :: Int }
    | AccountLoginFailed
        { reason :: AccountLoginFailReason }

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
          serverListHostAddress :: HostAddress
        }
    deriving Show

build :: TxPacket -> RawPacket
-- [0x82] AccountLoginFailed - 2 bytes long
build AccountLoginFailed{..} =
    RawPacket 0x82 (B.pack [0x82, reasonCode])
    where reasonCode = case reason of
                           IncorrectNameOrPassword           -> 0x00
                           SomeoneIsAlreadyUsingAccount      -> 0x01
                           YourAccountHasBeenBlocked         -> 0x02
                           YourAccountCredentialsAreInvalid  -> 0x03
                           CommunicationProblem              -> 0x04
-- [0x8C] ServerRedirect - 11 bytes long
build ServerRedirect{..} =
    assert (L.length raw == 11)   
    RawPacket 0x8C (lazy2strict raw)
    where raw = runPut $ do
              put (0x8C :: Word8) -- packet id
              putWord32be redirectHostAddress -- host address
              put (fromIntegral redirectHostPort :: Word16) -- host port
              put (fromIntegral encryptionKey :: Word32) -- encryption key
-- [0xA8] ServerList - dynamic length
build ServerList{..} =
    assert (L.length raw == fromIntegral pLen)   
    RawPacket 0xA8 (lazy2strict raw)
    where numServers = length servers
          pLen = 6 + (numServers * 40)
          rawTop = runPut $ do
              put (0xA8 :: Word8) -- packet id
              put (fromIntegral pLen :: Word16) -- packet length
              put (0x00 :: Word8) -- flags
              put (fromIntegral numServers :: Word16) -- server count
          rawServer (ServerListItem{..}, idx) = runPut $ do
              put (idx :: Word16) -- server index
              mapM put (truncString name 32) -- name of server
              put (fromIntegral percentFull :: Word8) -- percentage full
              put (fromIntegral timeZone :: Word8) -- timezone
              putWord32be serverListHostAddress -- host address
          rawServers = map rawServer (zip servers [0..])
          raw = L.concat (rawTop : rawServers)
