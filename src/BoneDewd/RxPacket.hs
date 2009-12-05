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
    | ClientVersion String
    | AccountLoginRequest
        { acctUser :: String,
          acctPass :: String }
    | GameLoginRequest
        { keyUsed :: Word32,
          acctUser :: String,
          acctPass :: String }
    | GetPlayerStatus
        { pType :: Word8,
          pSerial :: Word32 }
    | CharacterLoginRequest
        { charName :: String,
          charClientFlag :: Word32,
          charLoginCount :: Word32,
          charSlotChosen :: Word32,
          charClientIp :: Word32 }
    deriving Show

parse :: SessionState -> RawPacket -> RxPacket
parse PreGameLoginState (RawPacket raw) =
    ClientAuthKey (runGet getWord32be (strict2lazy raw))
parse _ (RawPacket raw) =
    parseApp pid raw
    where pid = runGet getWord8 (strict2lazy raw)
    
parseApp :: Word8 -> B.ByteString -> RxPacket
-- [0x34] GetPlayerStatus
parseApp 0x34 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 5
              t <- getWord8 -- type
              s <- getWord32be
              return $ GetPlayerStatus t s
-- [0x5D] CharacterLoginRequest
parseApp 0x5D raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 5
              n <- getFixedStringNul 30 -- char name
              skip 2
              f <- getWord32be
              skip 4
              c <- getWord32be
              skip 16
              s <- getWord32be
              i <- getWord32be
              return $ CharacterLoginRequest n f c s i
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
-- [0xBD] ClientVersion
parseApp 0xBD raw = 
    runGet getter (strict2lazy raw)
    where plen = B.length raw
          getter = do
              skip 3
              s <- getFixedStringNul (plen - 3) -- version string
              return (ClientVersion s)
-- [0xBF] ClientVersion
parseApp 0xBF raw = 
    runGet getter (strict2lazy raw)
    where plen = B.length raw
          getter = do
              skip 3
              subcmd <- getWord16be
              case subcmd of
                  _ -> error ("don't know how to parse subcommand of 0xBF: " ++ show subcmd ++ "\n" ++ fmtHex raw)
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
