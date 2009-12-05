{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module BoneDewd.RxPacket where
import Control.Applicative ((<$>))

--import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B
import Data.Word
import BoneDewd.RawPacket
import BoneDewd.Types
import BoneDewd.Util

data RxPacket
    = ServerSelect Int
    | ClientAuthKey Word32
    | ClientLoginSeed
        { seed :: Word32,
          verMajor :: Word32,
          verMinor :: Word32,
          verRev :: Word32,
          verProto :: Word32 }
    | AccountLoginRequest
        { acctUser :: String,
          acctPass :: String }
    | GameLoginRequest
        { keyUsed :: Word32,
          acctUser :: String,
          acctPass :: String }
    deriving Show

parse :: SessionState -> RawPacket -> RxPacket
parse PreGameLoginState (RawPacket raw) =
    ClientAuthKey (runGet getWord32be (strict2lazy raw))
parse _ (RawPacket raw) =
    parseApp pid raw
    where pid = runGet getWord8 (strict2lazy raw)
    
parseApp :: Word8 -> B.ByteString -> RxPacket
-- [0x80] AccountLoginRequest
parseApp 0x80 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              u <- getFixedStringNul 30 -- user
              p <- getFixedStringNul 30 -- password
              return (AccountLoginRequest u p)
-- [0x91] GameLoginRequest
parseApp 0x91 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              k <- getWord32be -- key used
              u <- getFixedStringNul 30 -- user
              p <- getFixedStringNul 30 -- password
              return (GameLoginRequest k u p)
-- [0xA0] ServerSelect
parseApp 0xA0 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              i <- getWord8 -- index of server that was selected   
              return (ServerSelect (fromIntegral i))
-- [0xEF] ClientLoginSeed
parseApp 0xEF raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              seed <- getWord32be
              verA <- getWord32be
              verB <- getWord32be
              verC <- getWord32be
              verD <- getWord32be
              return (ClientLoginSeed seed verA verB verC verD)
parseApp pid raw = error ("don't know how to parse packet: " ++ show pid ++ "\n" ++ fmtHex raw)
