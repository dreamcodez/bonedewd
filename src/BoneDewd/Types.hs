{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module BoneDewd.Types where
import Control.Concurrent.Chan
import Prelude hiding (Either(..))
import BoneDewd.Util
import qualified Data.ByteString as B
import Data.Bits
import Data.Int
import Data.Word
import System.IO (Handle(..))

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
    toEnum i =
        case (0x0F .&. i) of
            0x00 -> DirNorth
            0x01 -> DirRight
            0x02 -> DirEast
            0x03 -> DirDown
            0x04 -> DirSouth
            0x05 -> DirLeft
            0x06 -> DirWest
            0x07 -> DirUp

data MobDirection
    = MobDirection Direction RunningOrWalking
    deriving Show

instance Enum MobDirection where
    fromEnum (MobDirection d r) = fromEnum d .|. fromEnum r
    toEnum i = MobDirection (toEnum i) (toEnum i)
          
data RunningOrWalking
    = Running
    | Walking
    deriving Show

instance Enum RunningOrWalking where
    fromEnum Running = 0x80
    fromEnum Walking = 0x00
    toEnum i =
        case (0x80 .&. i) of
            0x80 -> Running
            0x00 -> Walking

data SpeechFont
    = SpeechFont { unSpeechFont :: Word16 }
    deriving Show
    
data Hue
    = Hue { unHue :: Word16 }
    deriving Show
    
data Language
    = Language { unLanguage :: String }
    deriving Show

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

data WarMode
    = Peace
    | War
    deriving Show
    

instance Enum WarMode where
    fromEnum Peace = 0x00
    fromEnum War   = 0x01
    toEnum 0x00 = Peace
    toEnum 0x01 = War

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
    
data Season
    = Spring
    | Summer
    | Fall
    | Winter
    | Desolation
    deriving Show

instance Enum Season where
    fromEnum Spring     = 0x00
    fromEnum Summer     = 0x01
    fromEnum Fall       = 0x02
    fromEnum Winter     = 0x03
    fromEnum Desolation = 0x04
    toEnum 0x00 = Spring
    toEnum 0x01 = Summer
    toEnum 0x02 = Fall
    toEnum 0x03 = Winter
    toEnum 0x04 = Desolation

data MobEquipmentItem
    = MobEquipmentItem
        { equipSerial :: Serial,
          equipGraphic :: Word16,
          equipLayer :: Word8,
          equipHue :: Word16 }
    deriving Show

data Loc
    = Loc
        { locX :: Word16,
          locY :: Word16,
          locZ :: Int8 }
    deriving Show

data Mobile
    = Mobile
        { mobSerial :: Serial,
          mobBody :: Word16,
          mobHue :: Word16,
          mobLoc :: Loc,
          mobDirection :: MobDirection,
          mobStatus :: MobStatus,
          mobNotoriety :: MobNotoriety,
          mobEquipment :: [MobEquipmentItem]
        }
    deriving Show
    
newtype Serial
    = Serial { unSerial :: Word32 }
    deriving Show

data MobileStats
    = MobileStats
        { statStr :: Word16,
          statDex :: Word16,
          statInt :: Word16,
          statCap :: Word16, -- stat cap
          statLuck :: Word16, -- special luck stat
          statCurHits :: Word16, -- hitpoints
          statMaxHits :: Word16,
          statCurMana :: Word16, -- mana
          statMaxMana :: Word16,
          statCurStam :: Word16, -- stamina
          statMaxStam :: Word16,
          statCurWeight :: Word16, -- weight
          statMaxWeight :: Word16,
          statCurFollow :: Word8, -- current amount of pets
          statMaxFollow :: Word8, -- max amount of pets
          statGold :: Word32, -- amount of gold in pack
          statMinDmg :: Word16,
          statMaxDmg :: Word16,
          statResistPhysical :: Word16,
          statResistFire :: Word16,
          statResistCold :: Word16,
          statResistPoison :: Word16,
          statResistEnergy :: Word16 }
    deriving Show

data RawPacket =
    RawPacket B.ByteString

instance Show RawPacket where
    show (RawPacket raw) = fmtHex raw

data Player
    = Player Mobile MobileStats
    deriving Show
    
data ParseState
    = LoginState
    | PreGameState
    | GameState 
    deriving Show
    
data WorldState
    = WorldState
    deriving Show
    
data Event
    = Ev
    deriving Show

-- the world channel, send events here...
data WorldChan
    = WorldChan (Chan Event)

instance Show WorldChan where
    show _ = "WorldChan"

data PacketEncoding
    = Compressed
    | NotCompressed

newtype Gump
    = Gump { unGump :: Word16 }
    deriving Show

data SpeechType
    = NormalSpeech
    | BroadcastSpeech
    | EmoteSpeech
    | SystemSpeech
    | MessageSpeech
    | YellSpeech
    | SpellSpeech
    | GuildSpeech
    | AllianceSpeech
    | CommandSpeech
    deriving Show

instance Enum SpeechType where
    fromEnum NormalSpeech    = 0x00
    fromEnum BroadcastSpeech = 0x01
    fromEnum EmoteSpeech     = 0x02
    fromEnum SystemSpeech    = 0x06
    fromEnum MessageSpeech   = 0x07
    fromEnum YellSpeech      = 0x09
    fromEnum SpellSpeech     = 0x0A
    fromEnum GuildSpeech     = 0x0D
    fromEnum AllianceSpeech  = 0x0E
    fromEnum CommandSpeech   = 0x0F
    toEnum b = case 0x0F .&. b of -- remove non-relevant flag bits
                   0x00 -> NormalSpeech
                   0x01 -> BroadcastSpeech
                   0x02 -> EmoteSpeech
                   0x06 -> SystemSpeech
                   0x07 -> MessageSpeech
                   0x09 -> YellSpeech
                   0x0A -> SpellSpeech
                   0x0D -> GuildSpeech
                   0x0E -> AllianceSpeech
                   0x0F -> CommandSpeech
