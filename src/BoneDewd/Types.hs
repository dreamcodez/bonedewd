{-# LANGUAGE RecordWildCards #-}
module BoneDewd.Types where
import Prelude hiding (Either(..))
import BoneDewd.Util
import qualified Data.ByteString as B
import Data.Bits
import Data.Int
import Data.Word

data Direction
    = DirNorth
    | DirRight
    | DirEast
    | DirDown
    | DirSouth
    | DirLeft
    | DirWest
    | DirUp
    deriving Show

instance Enum Direction where
    fromEnum DirNorth = 0x00
    fromEnum DirRight = 0x01
    fromEnum DirEast  = 0x02
    fromEnum DirDown  = 0x03
    fromEnum DirSouth = 0x04
    fromEnum DirLeft  = 0x05
    fromEnum DirWest  = 0x06
    fromEnum DirUp    = 0x07
    
data MobDirection
    = MobDirection Direction RunningOrWalking
    deriving Show

instance Enum MobDirection where
    fromEnum (MobDirection d r) = fromEnum d .|. fromEnum r
          
data RunningOrWalking
    = Running
    | Walking
    deriving Show

instance Enum RunningOrWalking where
    fromEnum Running = 0x80
    fromEnum Walking = 0x00

data MobStatus
    = MobStatus
        { mobCanAlterPaperDoll :: Bool,
          mobPoisoned :: Bool,
          mobGoldHealth :: Bool,
          mobWarMode :: Bool }
    deriving Show

instance Enum MobStatus where
    fromEnum MobStatus{..} =
        a .|. b .|. c .|. d
        where a | mobCanAlterPaperDoll = 0x02
                | otherwise            = 0x00
              b | mobPoisoned          = 0x04
                | otherwise            = 0x00
              c | mobGoldHealth        = 0x08
                | otherwise            = 0x00
              d | mobWarMode           = 0x40
                | otherwise            = 0x00

data MobNotoriety
    = Innocent -- Blue
    | Friend -- Green
    | Animal -- Grey
    | Criminal -- Grey
    | Enemy -- Orange
    | Murderer -- Red
    | Invulnerable -- Yellow
    deriving Show

instance Enum MobNotoriety where
    fromEnum Innocent     = 0x01
    fromEnum Friend       = 0x02
    fromEnum Animal       = 0x03
    fromEnum Criminal     = 0x04
    fromEnum Enemy        = 0x05
    fromEnum Murderer     = 0x06
    fromEnum Invulnerable = 0x07

data MobEquipmentItem
    = MobEquipmentItem
        { equipSerial :: Word32,
          equipGraphic :: Word16,
          equipLayer :: Word8,
          equipHue :: Word16 }
    deriving Show

data Mobile
    = Mobile
        { mobSerial :: Word32,
          mobBody :: Word16,
          mobHue :: Word16,
          mobX :: Int16,
          mobY :: Int16,
          mobZ :: Int8,
          mobDirection :: MobDirection,
          mobStatus :: MobStatus,
          mobNotoriety :: MobNotoriety,
          mobEquipment :: [MobEquipmentItem]
        }
    deriving Show

data RawPacket =
    RawPacket B.ByteString

instance Show RawPacket where
    show (RawPacket raw) = fmtHex raw

data SessionState
    = AccountLoginState
    | PreGameLoginState
    | GameLoginState
    deriving Show
