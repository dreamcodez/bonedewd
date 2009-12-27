{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards #-}
module BoneDewd.Types where
import BoneDewd.Util
import Control.Concurrent.Chan
import qualified Data.ByteString as B
import Data.Bits
import Data.Int
import qualified Data.Map as M
import Data.Text as T
import Data.Word
import Network (HostName)
import Network.Socket (HostAddress)
import Prelude hiding (Either(..))
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
    = Player
        { playerClient :: Client,
          playerMobile :: Mobile,
          playerStats :: MobileStats -- only if logged into a char
        }
    deriving Show
    
data ParseState
    = LoginState
    | PreGameState
    | GameState 
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
    | WhisperSpeech
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
    fromEnum WhisperSpeech   = 0x08
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
                   0x08 -> WhisperSpeech
                   0x09 -> YellSpeech
                   0x0A -> SpellSpeech
                   0x0D -> GuildSpeech
                   0x0E -> AllianceSpeech
                   0x0F -> CommandSpeech

data WorldState
    = WorldState
        { worldPlayers :: M.Map ClientId Player }
emptyWorldState = WorldState M.empty

newtype ClientId = ClientId Integer deriving (Eq,Num,Ord,Show)

data Client
    = Client
        { clientId :: ClientId,
          clientHost :: HostName,
          clientWrite :: TxPacket -> IO (),
          clientDisconnect :: IO ()
        }
instance Show Client where
    show Client{clientId,clientHost} =
        "(Client (" ++ show clientId ++ ") " ++ show clientHost ++ ")"  

data ClientManagerState
    = ClientManagerState
        { mgrClients :: M.Map ClientId Client,
          mgrNextClientId :: ClientId,
          mgrWorldChan :: Chan (Client,RxPacket)
        }

data RxPacket
    = ServerSelect Int
    | ChatButton
    | ClientAuthKey Word32
    | ClientLanguage String
    | ClientLoginSeed
        { seed :: Word32,
          verMajor :: Word32,
          verMinor :: Word32,
          verRev :: Word32,
          verProto :: Word32 }
    | ClientVersion String
    | ClosedStatusGump Serial
    | DisconnectNotification
    | AccountLoginRequest
        { acctUser :: String,
          acctPass :: String }
    | GameLoginRequest
        { keyUsed :: Word32,
          acctUser :: String,
          acctPass :: String }
    | GuildButton
    | HelpButton
    | IgnoredPacket
    | CharacterLoginRequest
        { charName :: String,
          charClientFlag :: Word32,
          charLoginCount :: Word32,
          charSlotChosen :: Word32,
          charClientIp :: Word32 }
    | LookRequest Serial
    | MoveRequest
        { moveDir :: MobDirection,
          moveSeq :: Word8,
          moveKey :: Word32 }
    | PaperDollRequest
    -- | 7.0.3.0: The client pings the server roughly every minute, sequence
    -- is always 0 in my testing
    | Ping Word8
    | PopupEntrySelection
        { charId :: Word32,
          entryId :: Word8 }
    | QuestButton
    | RequestStatus Serial
    | RequestSkills Serial
    | RequestWarMode WarMode
    | ScreenSize Word16 Word16
    | SpeechRequest SpeechType Hue SpeechFont Language T.Text
    | UseRequest Serial
    deriving Show

data TxPacket
    = CharacterListAfterDelete [CharacterListItem]
    | CharacterList
        { charListChars :: [CharacterListItem],
          charListCities :: [StartingCity] }
    | DrawContainer Serial Gump
    | DrawMobile Mobile
    | DrawPlayer Mobile
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
    | CharacterLoginComplete
    | CharacterLoginConfirm
        { loginMobile :: Mobile,
          mapWidth :: Word16,
          mapHeight :: Word16
        }
    | MoveAccept Word8 MobNotoriety
    | OpenPaperDoll Serial String MobStatus
    | Pong Word8
    | SendUnicodeSpeech
        { speechSerial :: Serial,
          speechType :: SpeechType,
          speechHue :: Hue,
          speechFont :: SpeechFont,
          speechLanguage :: Language,
          speechName :: String,
          speechText :: T.Text
        }
    | SetLightLevel Word8
    | SetSeason
        { season :: Season,
          seasonPlaySound :: Bool
        }
    | SetWarMode WarMode
    | SupportedFeatures Word32
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
        { charListName :: String,
          charListPass :: String
        }
    deriving Show

data StartingCity
    = StartingCity
        { cityName :: String,
          cityArea :: String
        }
    deriving Show

        