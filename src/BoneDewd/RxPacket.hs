{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module BoneDewd.RxPacket where
import Control.Applicative ((<$>))
--import Data.Binary
import Data.Binary.Get
import Data.Word
import BoneDewd.RawPacket
import BoneDewd.Util

data RxPacket
    = ServerSelect
        { serverIndex :: Int }
    | ClientLoginSeed
        { seed :: Word32,
          verMajor :: Word32,
          verMinor :: Word32,
          verRev :: Word32,
          verProto :: Word32 }
    | AccountLoginRequest
        { acctUser :: String,
          acctPass :: String }
    deriving Show

parse :: RawPacket -> RxPacket
-- [0x80] AccountLoginRequest
parse RawPacket{pktId=0x80,pktRaw} =
    runGet getter (strict2lazy pktRaw)
    where getter = do
              skip 1
              u <- getFixedStringNul 30 -- user
              p <- getFixedStringNul 30 -- password
              return (AccountLoginRequest u p)
-- [0xA0] ServerSelect
parse RawPacket{pktId=0xA0,pktRaw} =
    runGet getter (strict2lazy pktRaw)
    where getter = do
              skip 1
              i <- getWord8 -- index of server that was selected   
              return (ServerSelect (fromIntegral i))
-- [0xEF] ClientLoginSeed
parse RawPacket{pktId=0xEF,pktRaw} =
    runGet getter (strict2lazy pktRaw)
    where getter = do
              skip 1
              seed <- getWord32be
              verA <- getWord32be
              verB <- getWord32be
              verC <- getWord32be
              verD <- getWord32be
              return (ClientLoginSeed seed verA verB verC verD)
parse RawPacket{..} = error ("don't know how to parse packet: " ++ show pktId)
