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
import Text.Printf

data RxPacket
    = ServerSelect Int
    | ClientAuthKey Word32
    | ClientLanguage String
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
    | PopupEntrySelection
        { charId :: Word32,
          entryId :: Word8 }
    | ScreenSize Word16 Word16
    deriving Show

parse :: SessionState -> RawPacket -> Either String RxPacket
parse PreGameLoginState (RawPacket raw) =
    Right $ ClientAuthKey (runGet getWord32be (strict2lazy raw))
parse _ (RawPacket raw) =
    parseApp pid raw
    where pid = runGet getWord8 (strict2lazy raw)
    
parseApp :: Word8 -> B.ByteString -> Either String RxPacket
-- [0x34] GetPlayerStatus
parseApp 0x34 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 5
              t <- getWord8 -- type
              s <- getWord32be
              return $ Right (GetPlayerStatus t s)
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
              return $ Right (CharacterLoginRequest n f c s i)
-- [0x80] AccountLoginRequest
parseApp 0x80 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              u <- getFixedStringNul 30 -- user
              p <- getFixedStringNul 30 -- password
              return $ Right (AccountLoginRequest u p)
-- [0x91] GameLoginRequest
parseApp 0x91 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              k <- getWord32be -- key used
              u <- getFixedStringNul 30 -- user
              p <- getFixedStringNul 30 -- password
              return $ Right (GameLoginRequest k u p)
-- [0xA0] ServerSelect
parseApp 0xA0 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              i <- getWord8 -- index of server that was selected   
              return $ Right (ServerSelect (fromIntegral i))
-- [0xBD] ClientVersion
parseApp 0xBD raw = 
    runGet getter (strict2lazy raw)
    where plen = B.length raw
          getter = do
              skip 3
              s <- getFixedStringNul (plen - 3) -- version string
              return $ Right (ClientVersion s)
-- [0xBF]
parseApp 0xBF raw = 
    runGet getter (strict2lazy raw)
    where plen = B.length raw
          getter = do
              skip 3
              subcmd <- getWord16be
              case subcmd of
                  0x05 -> do -- ScreenSize
                      skip 2
                      x <- getWord16be
                      y <- getWord16be
                      return $ Right (ScreenSize x y)
                  0x0B -> do
                      l <- getFixedString 3
                      return $ Right (ClientLanguage l)
                  0x0F -> do
                      cid <- getWord32be
                      eid <- getWord8 -- i think this is right, penultima says word16 tho..
                      return $ Right (PopupEntrySelection cid eid)
                  _ -> return $ Left ("don't know how to parse subcommand of 0xBF: " ++ printf "%02x" subcmd)
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
              return $ Right (ClientLoginSeed seed verA verB verC verD)
parseApp pid raw = Left ("don't know how to parse packet: " ++ printf "0x%02x" pid)
