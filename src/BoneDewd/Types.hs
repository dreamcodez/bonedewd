module BoneDewd.Types where
import BoneDewd.Util
import qualified Data.ByteString as B

data RawPacket =
    RawPacket B.ByteString

instance Show RawPacket where
    show (RawPacket raw) = fmtHex raw

data SessionState
    = AccountLoginState
    | PreGameLoginState
    | GameLoginState
    deriving Show
