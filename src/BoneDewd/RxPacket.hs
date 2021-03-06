{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module BoneDewd.RxPacket where
import Control.Applicative ((<$>))

import Data.Binary.Get
import Data.Bits
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString as B
import Data.Word
import BoneDewd.Types
import BoneDewd.Util
import Text.Printf

parse :: ParseState -> RawPacket -> Either String (ParseState,RxPacket)
parse PreGameState (RawPacket raw) =
    Right $ (GameState,ClientAuthKey (runGet getWord32be (strict2lazy raw)))
parse state (RawPacket raw) =
    case parseApp pid raw of
        Right pkt -> Right (state,pkt)
        Left err  -> Left err
    where pid = runGet getWord8 (strict2lazy raw)
    
parseApp :: Word8 -> B.ByteString -> Either String RxPacket
-- [0x02] MoveRequest
parseApp 0x02 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              d <- getWord8
              s <- getWord8
              k <- getWord32be
              return $ Right (MoveRequest (toEnum (fromIntegral d) :: MobDirection) s k)
-- [0x06] UseRequest / PaperDollRequest
parseApp 0x06 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              res <- getWord32be
              if (res .&. 0x80000000) == 0x80000000
                  then return $ Right PaperDollRequest
                  else return $ Right (UseRequest (Serial res))
-- [0x09] LookRequest
parseApp 0x09 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              Right . LookRequest . Serial <$> getWord32be
-- [0x2C] IgnoredPacket (Resurrection Menu? / RunUO ignores it)
parseApp 0x2C _ = Right IgnoredPacket
-- [0x34] RequestStatus / RequestSkills
parseApp 0x34 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 5
              t <- getWord8 -- type
              s <- Serial <$> getWord32be
              return $ case t of
                           0x04 -> Right (RequestStatus s)
                           0x05 -> Right (RequestSkills s)
                           _    -> Left "unknown type for app packet 0x34"
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
-- [0x72] RequestWarMode
parseApp 0x72 raw = 
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              Right . RequestWarMode . toEnum . fromIntegral <$> getWord8
-- [0x73] Ping
parseApp 0x73 raw =
    Right (Ping seqid) 
    where seqid = runGet (skip 1 >> getWord8) (strict2lazy raw)
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
-- [0x9B] HelpButton
parseApp 0x9B _ = Right HelpButton
-- [0xA0] ServerSelect
parseApp 0xA0 raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 1
              i <- getWord8 -- index of server that was selected   
              return $ Right (ServerSelect (fromIntegral i))
-- [0xAD] SpeechRequest
parseApp 0xAD raw =
    runGet getter (strict2lazy raw)
    where getter = do
              skip 3
              rawType <- getWord8 -- rawtype, need to know if its 0xc0
              let t = toEnum (fromIntegral rawType) -- speech type
              hue <- Hue <$> getWord16be -- color
              font <- SpeechFont <$> getWord16be -- font
              lang <-  Language <$> getFixedStringNul 4 -- language
              if rawType .&. 0xC0 == 0xC0
                  -- need to parse keywords + ascii, then normalize to unicode
                  then do
                      keyLen <- getWord16be
                      let keyLen' = shiftR keyLen 4 -- real len is 12 bit int
                          keyBitsLeft = keyLen' * 12 - 4
                          keyBytesLeft = (keyBitsLeft + (keyBitsLeft `mod` 8)) `div` 8
                          strLen = B.length raw - (fromIntegral (14 + keyBytesLeft))
                      -- skip the remaining keyword bytes
                      skip (fromIntegral keyBytesLeft)
                      txt <- decodeASCII <$> getFixedByteString strLen
                      -- decode leaves the nul, so need to take it out...
                      let txt' = T.take (T.length txt - 1) txt
                      return $ Right (SpeechRequest t hue font lang txt')
                  -- parse in standard unicode fashion
                  else do
                      let strLen = B.length raw - 12
                      txt <- decodeUtf16BE <$> getFixedByteString strLen
                      -- decode leaves the nul, so need to take it out...
                      let txt' = T.take (T.length txt - 1) txt
                      return $ Right (SpeechRequest t hue font lang txt')
-- [0xB5] ChatButton
parseApp 0xB5 _ = Right ChatButton
-- [0xBD] ClientVersion
parseApp 0xBD raw = 
    runGet getter (strict2lazy raw)
    where plen = B.length raw
          getter = do
              skip 3
              s <- getFixedStringNul (plen - 3) -- version string
              return $ Right (ClientVersion s)
-- [0xBF] generalized packet
parseApp 0xBF raw = 
    runGet getter (strict2lazy raw)
    where _plen = B.length raw
          getter = do
              skip 3
              subcmd <- getWord16be
              case subcmd of
                  0x05 -> do -- ScreenSize
                      skip 2
                      x <- getWord16be
                      y <- getWord16be
                      return $ Right (ScreenSize x y)
                  0x0B -> do -- ClientLanguage
                      l <- getFixedString 3
                      return $ Right (ClientLanguage l)
                  0x0C -> do -- ClosedStatusGump
                      s <- Serial <$> getWord32be
                      return $ Right (ClosedStatusGump s)
                  0x0F -> do -- PopupEntrySelection
                      cid <- getWord32be
                      eid <- getWord8 -- i think this is right, penultima says word16 tho..
                      return $ Right (PopupEntrySelection cid eid)
                  0x24 -> do -- unknown. UOSE Introduced (http://docs.polserver.com/packets/index.php?Packet=0xBF)
                      return (Right IgnoredPacket)
                  _ -> return $ Left ("don't know how to parse subcommand of 0xBF: " ++ printf "0x%02x" subcmd ++ "\n" ++ fmtHex raw)
-- [0xD7] generalized AOS packet
parseApp 0xD7 raw = 
    runGet getter (strict2lazy raw)
    where _plen = B.length raw
          getter = do
              skip 3
              _pserial <- getWord32be
              subcmd <- getWord16be
              case subcmd of
                  0x28 -> do -- GuildButton
                      return (Right GuildButton)
                  0x32 -> do -- QuestButton
                      return (Right QuestButton)
                  _ -> return $ Left ("don't know how to parse subcommand of 0xD7: " ++ printf "0x%02x" subcmd ++ "\n" ++ fmtHex raw)
-- [0xD9] IgnoredPacket (unused system/video/hw info)
parseApp 0xD9 _ = Right IgnoredPacket
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
parseApp pid raw = Left ("don't know how to parse packet: " ++ printf "0x%02X" pid ++ "\n" ++ fmtHex raw)
