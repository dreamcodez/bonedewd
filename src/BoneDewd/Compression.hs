{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module BoneDewd.Compression where
import qualified Data.ByteString as B
import Data.Word
import Foreign.C
import Foreign.Marshal.Array
import System.IO.Unsafe
import System.Log.Logger

{-
while ( packetPos < packetSize )
	{
		Q_UINT8 packetByte = static_cast<Q_UINT8>( rawPacket.at( packetPos++ ) );
		codeSize = bitTable[packetByte].size;
		code = bitTable[packetByte].code;
		buffer32 <<= codeSize;
		buffer32 |= code;
		bufferSize += codeSize;
		while ( bufferSize >= 8 ) // do we have 1 byte ready?
		{
			bufferSize -= 8;
			temp[actByte++] = ( unsigned char ) ( buffer32 >> bufferSize ) & 0xFF;
		}
	}
	codeSize = bitTable[256].size;
	code = bitTable[256].code;
	buffer32 <<= codeSize;
	buffer32 |= code;
	bufferSize += codeSize;
	while ( bufferSize >= 8 )
	{
		bufferSize -= 8;
		temp[actByte++] = ( unsigned char ) ( buffer32 >> bufferSize ) & 0xFF;//31;
	}
	if ( bufferSize > 0 )
	{
		temp[actByte++] = ( unsigned char ) ( buffer32 << 8 - bufferSize ) & 0xFF;//& 31;
	}
-}

-- compress :: B.ByteString -> B.ByteString
compress :: B.ByteString -> B.ByteString
compress input = unsafePerformIO $ do
    B.useAsCStringLen input $ \(inputBuf,inputLen) -> do
    let outputMaxLen = inputLen * 2
    allocaArray outputMaxLen $ \outputBuf -> do
    outputLen <- c_compress inputBuf (fromIntegral inputLen) outputBuf (fromIntegral outputMaxLen)
    debugM "Compression" (show inputLen ++ " bytes before compression / " ++ show outputLen ++ " bytes after")
    B.packCStringLen (outputBuf,fromIntegral outputLen)

foreign import ccall "compress" c_compress :: CString -> CInt -> CString -> CInt -> IO CInt
