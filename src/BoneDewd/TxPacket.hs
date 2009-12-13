{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module BoneDewd.TxPacket where
import Control.Exception
import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Int
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.Socket (HostAddress)
import BoneDewd.Types
import BoneDewd.Util

data TxPacket
    = CharacterListAfterDelete [CharacterListItem]
    | CharacterList
        { characters :: [CharacterListItem],
          cities :: [StartingCity] }
    | DrawPlayer Mobile
    | DrawMobile Mobile
    | ServerList
        { servers :: [ServerListItem] }
    | ServerRedirect
        { redirectHostAddress :: HostAddress,
          redirectHostPort :: Int,
          encryptionKey :: Int }
    | StatusBarInfo
        { sbSerial :: Serial,
          sbName :: String,
          sbStats :: MobileStats,
          sbTithe :: Word32, -- tithing points
          sbCanChangeName :: Bool
        }
    | AccountLoginFailed
        { reason :: AccountLoginFailReason }
    | LoginComplete
    | LoginConfirm
        { loginMobile :: Mobile,
          mapWidth :: Word16,
          mapHeight :: Word16
        }
    | MoveAccept Word8 MobNotoriety
    | OpenPaperDoll Serial String MobStatus
    | Pong Word8
    | SetWarMode WarMode
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
-- [0x11] StatusBarInfo - dynamic length
-- I chose to standardize on the ML feature set (but not kr)
build StatusBarInfo{sbSerial,sbName,sbStats,sbTithe,sbCanChangeName} =
    assert (L.length raw == fromIntegral pLen)
    RawPacket (lazy2strict raw)
    where pLen = 91
          raw = putter sbStats
          putter MobileStats{..} = runPut $ do
              putWord8 0x11
              putWord16be (fromIntegral pLen)
              putWord32be (unSerial sbSerial)
              mapM_ put (truncString sbName 30) -- name of mobile on status bar
              putWord16be statCurHits
              putWord16be statMaxHits
              if sbCanChangeName
                  then putWord8 0x01
                  else putWord8 0x00
              putWord8 0x05 -- UOML Extended Info
              putWord8 0x00 -- male human
              putWord16be statStr
              putWord16be statDex
              putWord16be statInt
              putWord16be statCurStam
              putWord16be statMaxStam
              putWord16be statCurMana
              putWord16be statMaxMana
              putWord32be statGold
              putWord16be statResistPhysical
              putWord16be statCurWeight
              putWord16be statMaxWeight
              putWord8 0x01 -- human race
              putWord16be statCap
              putWord8 statCurFollow
              putWord8 statMaxFollow
              putWord16be statResistFire
              putWord16be statResistCold
              putWord16be statResistPoison
              putWord16be statResistEnergy
              putWord16be statLuck
              putWord16be statMinDmg
              putWord16be statMaxDmg
              putWord32be sbTithe

-- [0x1B] LoginConfirm - 37 bytes long
build LoginConfirm{..} =
    assert (L.length raw == fromIntegral pLen)
    RawPacket (lazy2strict raw)
    where pLen = 37
          raw = runPut $ do
              put (0x1B :: Word8) -- packet id
              put (unSerial $ mobSerial loginMobile :: Word32) -- serial # of mob
              put (0x00 :: Word32) -- unknown, always 0
              put (mobBody loginMobile :: Word16)
              put ((locX $ mobLoc loginMobile) :: Word16)
              put ((locY $ mobLoc loginMobile) :: Word16)
              put (0x00 :: Word8)
              put ((locZ $ mobLoc loginMobile) :: Int8)
              put (fromIntegral (fromEnum (mobDirection loginMobile)) :: Word8)
              put (0x00 :: Word32) -- unknown, always 0
              put (0x00 :: Word32) -- unknown, always 0
              put (0x00 :: Word8) -- unknown, always 0
              put (mapWidth :: Word16) -- map width minus 8
              put (mapHeight :: Word16) -- map height
              put (0x00 :: Word16) -- unknown, always 0
              put (0x00 :: Word32) -- unknown, always 0
-- [0x20] DrawPlayer - 19 bytes long
build (DrawPlayer Mobile{..}) =
    assert (L.length raw == fromIntegral pLen)
    RawPacket (lazy2strict raw)
    where pLen = 19
          raw = runPut $ do
              put (0x20 :: Word8) -- packet id
              put (unSerial mobSerial :: Word32) -- serial # of mob
              put (mobBody :: Word16)
              put (0x00 :: Word8) -- unknown
              put (mobHue :: Word16) -- hue
              put (0x00 :: Word8) -- flag
              put (locX mobLoc :: Word16)
              put (locY mobLoc :: Word16)
              put (0x00 :: Word16) -- unknown, always 0
              put (fromIntegral (fromEnum mobDirection) :: Word8)
              put (locZ mobLoc :: Int8)
-- [0x22] MoveAccept - 3 bytes long
-- sent in response to a MoveRequest
build (MoveAccept seqId n) =
    RawPacket (lazy2strict raw)
    where raw = runPut $ do
              putWord8 0x22
              putWord8 seqId
              putWord8 (fromIntegral(fromEnum n))
-- [0x55] LoginComplete - 1 byte long
build LoginComplete =
    RawPacket (lazy2strict (runPut (putWord8 0x55)))
-- [0x72] SetWarMode - 5 bytes long
build (SetWarMode m) =
    RawPacket (lazy2strict raw)
    where raw = runPut $ do
              putWord8 0x72
              putWord8 (fromIntegral (fromEnum m)) -- warmode flag
              -- always these values according to http://docs.polserver.com/packets/index.php?Packet=0x72
              putWord8 0x00
              putWord8 0x32
              putWord8 0x00
-- [0x73] Ping - 2 bytes long
build (Pong seqid) =
    RawPacket (lazy2strict raw)
    where raw = runPut $ do
              putWord8 0x73
              putWord8 seqid
-- [0x78] DrawMobile - dynamic length
build (DrawMobile Mobile{..}) =
    assert (L.length raw == fromIntegral pLen)
    RawPacket (lazy2strict raw)
    where pLen = 23 + (length mobEquipment) * 9
          raw = runPut $ do
              put (0x78 :: Word8) -- packet id
              put (fromIntegral pLen :: Word16) -- length
              put (unSerial mobSerial :: Word32) -- serial # of mob
              put (mobBody :: Word16)
              put (locX mobLoc :: Word16)
              put (locY mobLoc :: Word16)
              put (locZ mobLoc :: Int8)
              put (fromIntegral (fromEnum mobDirection) :: Word8)
              put (mobHue :: Word16) -- hue
              put (fromIntegral (fromEnum mobStatus) :: Word8)
              put (fromIntegral (fromEnum mobNotoriety) :: Word8)
              forM_ mobEquipment $ \MobEquipmentItem{..} -> do
                  put (equipSerial :: Word32)
                  put (equipGraphic :: Word16)
                  put (equipLayer :: Word8)
                  put (equipHue :: Word16)
              put (0x00 :: Word32) -- end marker
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
-- [0x88] OpenPaperDoll - 66 bytes long
build (OpenPaperDoll (Serial cid) title stat) = 
    assert (L.length raw == fromIntegral pLen)
    RawPacket (lazy2strict raw)
    where pLen = 66
          raw = runPut $ do
              putWord8 0x88
              putWord32be cid
              mapM_ put (truncString title 60)
              putWord8 (fromIntegral $ fromEnum stat)
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
