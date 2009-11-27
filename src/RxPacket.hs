{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module RxPacket where
import Control.Applicative ((<$>))
--import Data.Binary
import Data.Binary.Get
import RawPacket
import Util

data RxPacket
    = ServerSelect
        { serverIndex :: Int }
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
parse RawPacket{..} = error ("don't know how to parse packet: " ++ show pktId)
