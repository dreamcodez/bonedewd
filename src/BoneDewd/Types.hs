module BoneDewd.Types where
import Prelude hiding (Either(..))
import BoneDewd.Util
import qualified Data.ByteString as B
import Data.Bits
import Data.Int
import Data.Word

data Direction
    = North
    | Right
    | East
    | Down
    | South
    | Left
    | West
    | Up
    deriving Show

instance Enum Direction where
    fromEnum North = 0x00
    fromEnum Right = 0x01
    fromEnum East  = 0x02
    fromEnum Down  = 0x03
    fromEnum South = 0x04
    fromEnum Left  = 0x05
    fromEnum West  = 0x06
    fromEnum Up    = 0x07
    
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

data Mobile
    = Mobile
        { mobSerial :: Word32,
          mobBody :: Word16,
          mobX :: Int16,
          mobY :: Int16,
          mobZ :: Int16,
          mobDirection :: MobDirection
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
