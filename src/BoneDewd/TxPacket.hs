{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module BoneDewd.TxPacket where
import Control.Exception
import Data.Binary
import Data.Binary.Put
import Data.Int
import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.Socket (HostAddress)
import BoneDewd.RawPacket
import BoneDewd.Types
import BoneDewd.Util

data TxPacket
    = CharacterListAfterDelete [CharacterListItem]
    | CharacterList
        { characters :: [CharacterListItem],
          cities :: [StartingCity] }
    | ServerList
        { servers :: [ServerListItem] }
    | ServerRedirect
        { redirectHostAddress :: HostAddress,
          redirectHostPort :: Int,
          encryptionKey :: Int }
    | AccountLoginFailed
        { reason :: AccountLoginFailReason }
    | LoginConfirm
        { loginMobile :: Mobile,
          mapWidth :: Word16,
          mapHeight :: Word16
        }
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

data CharacterListItem
    = CharacterListItem
        { charName :: String,
          charPass :: String
        }
    deriving Show

data StartingCity
    = StartingCity
        { cityName :: String,
          cityArea :: String
        }
    deriving Show

build :: TxPacket -> RawPacket
-- [0x1B] LoginConfirm - 37 bytes long
build LoginConfirm{..} =
    assert (L.length raw == fromIntegral pLen)
    RawPacket (lazy2strict raw)
    where pLen = 37
          raw = runPut $ do
              put (0x1B :: Word8) -- packet id
              put (mobSerial loginMobile :: Word32) -- serial # of mob
              put (0x00 :: Word32) -- unknown, always 0
              put (mobBody loginMobile :: Word16)
              put (mobX loginMobile :: Int16)
              put (mobY loginMobile :: Int16)
              put (mobZ loginMobile :: Int16)
              put (fromIntegral (fromEnum (mobDirection loginMobile)) :: Word8)
              put (0x00 :: Word32) -- unknown, always 0
              put (0x00 :: Word32) -- unknown, always 0
              put (0x00 :: Word8) -- unknown, always 0
              put (mapWidth :: Word16) -- map width minus 8
              put (mapHeight :: Word16) -- map height
              put (0x00 :: Word16) -- unknown, always 0
              put (0x00 :: Word32) -- unknown, always 0
-- [0x82] AccountLoginFailed - 2 bytes long
build AccountLoginFailed{..} =
    RawPacket (B.pack [0x82, reasonCode])
    where reasonCode = case reason of
                           IncorrectNameOrPassword           -> 0x00
                           SomeoneIsAlreadyUsingAccount      -> 0x01
                           YourAccountHasBeenBlocked         -> 0x02
                           YourAccountCredentialsAreInvalid  -> 0x03
                           CommunicationProblem              -> 0x04
-- [0x86] CharacterListAfterDelete - dynamic length
build (CharacterListAfterDelete characters) =
    assert (numChars `elem` [5..7])
    assert (L.length raw == fromIntegral pLen)   
    RawPacket (lazy2strict raw)
    where numChars = length characters
          pLen = 4 + (numChars * 60)
          rawTop = runPut $ do
              put (0x86 :: Word8) -- packet id
              put (fromIntegral pLen :: Word16) -- packet length
              put (fromIntegral numChars :: Word8) -- char count
          rawChar CharacterListItem{..} = runPut $ do
              mapM_ put (truncString charName 30) -- name of char
              mapM_ put (truncString charPass 30) -- pass of char
          rawChars = map rawChar characters
          raw = L.concat (rawTop : rawChars)
-- [0x8C] ServerRedirect - 11 bytes long
build ServerRedirect{..} =
    assert (L.length raw == 11)   
    RawPacket (lazy2strict raw)
    where raw = runPut $ do
              put (0x8C :: Word8) -- packet id
              putWord32le redirectHostAddress -- host address
              putWord16be (fromIntegral redirectHostPort)
              --put (fromIntegral redirectHostPort :: Word16) -- host port
              put (fromIntegral encryptionKey :: Word32) -- encryption key
-- [0xA8] ServerList - dynamic length
build ServerList{..} =
    assert (L.length raw == fromIntegral pLen)   
    RawPacket (lazy2strict raw)
    where numServers = length servers
          pLen = 6 + (numServers * 40)
          rawTop = runPut $ do
              put (0xA8 :: Word8) -- packet id
              put (fromIntegral pLen :: Word16) -- packet length
              put (0x5D :: Word8) -- flags, 0x5D is what RunUO sends
              put (fromIntegral numServers :: Word16) -- server count
          rawServer (ServerListItem{..}, idx) = runPut $ do
              put (idx :: Word16) -- server index
              mapM put (truncString name 32) -- name of server
              put (fromIntegral percentFull :: Word8) -- percentage full
              put (fromIntegral timeZone :: Word8) -- timezone
              putWord32be serverListHostAddress -- host address
          rawServers = map rawServer (zip servers [0..])
          raw = L.concat (rawTop : rawServers)
-- [0xA9] CharacterList - dynamic length
build CharacterList{..} =
    assert (numChars' `elem` [5..7])
    assert (L.length raw == fromIntegral pLen)  
    RawPacket (lazy2strict raw)
    where -- client expects you to pad the 'empty' characters
          characters' = characters ++ (take (7 - numChars) (repeat (CharacterListItem "" "")))
          numChars = length characters
          numChars' = length characters'
          numCities = length cities
          pLen = 9 + (numChars' * 60) + (numCities * 63)
          rawTop = runPut $ do
              put (0xA9 :: Word8) -- packet id
              put (fromIntegral pLen :: Word16) -- packet length
              put (fromIntegral numChars' :: Word8) -- char count
          rawChar CharacterListItem{..} = runPut $ do
              mapM_ put (truncString charName 30) -- name of char
              mapM_ put (truncString charPass 30) -- pass of char
          rawChars = map rawChar characters'
          rawCityTop = runPut $ put (fromIntegral numCities :: Word8) -- city count
          rawCity (StartingCity{..}, idx) = runPut $ do
              put (idx :: Word8) -- city index
              mapM_ put (truncString cityName 31) -- name of city
              mapM_ put (truncString cityArea 31) -- area of city
          rawCities = map rawCity (zip cities [0..])
          rawBottom = runPut $ put (0x00 :: Word32) -- flags
          raw = L.concat ([rawTop] ++ rawChars ++ [rawCityTop] ++ rawCities ++ [rawBottom])
