{-# OPTIONS_GHC -Wall #-}
module BoneDewd.Util where
import Control.Applicative ((<$>))
import Control.Exception
import Data.Binary.Get
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int
import Network.Socket (Socket)
import Network.Socket.ByteString
import System.IO.Unsafe
import Text.Hexdump

recvExactly :: Socket -> Int -> IO B.ByteString
recvExactly peer nbytes
    | nbytes <= 0 = return B.empty
	| otherwise = do res <- recv peer nbytes
	                 B.append res <$> recvExactly peer (nbytes - B.length res)

fmtHex :: String -> BC.ByteString -> String
fmtHex title dta =
  title ++ "\n" ++
  "-----------------------------------------------------------------------------\n" ++
  hexdump 0 (BC.unpack dta) ++ "\n"

lazy2strict :: L.ByteString -> B.ByteString
lazy2strict lazy = B.concat (L.toChunks lazy)

strict2lazy :: B.ByteString -> L.ByteString
strict2lazy strict = L.fromChunks [strict]

-- truncate string to given length and add nul termination
-- guaranteed to be of the exact length specified (will pad)
truncString :: String -> Int -> String
truncString str len =
    str' ++ padding
    where str' = (take (len - 1) str) ++ "\0"
          padding = replicate (len - length str') '\0'

-- like getLazyByteStringNul except will consume up to fixed amount (and discard past nul)
getFixedByteStringNul :: Int -> Get B.ByteString
getFixedByteStringNul len = do
    buf <- getByteString len
    return (B.takeWhile (/= 0) buf)

getFixedStringNul :: Int -> Get String
getFixedStringNul len = BC.unpack <$> getFixedByteStringNul len
