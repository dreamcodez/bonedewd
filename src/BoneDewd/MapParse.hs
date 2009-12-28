{-# OPTIONS_GHC -Wall #-}
module BoneDewd.MapParse (getElevation) where
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.Binary.Strict.Get
import Data.Int
import Data.Word
import System.FilePath ((</>))
import System.IO (IOMode(..), SeekMode(..), hSeek, withBinaryFile)

getElevation :: (Word16,Word16) -> IO Int8
getElevation (x,y) = do
    withBinaryFile ("mul" </> "map0.mul") ReadMode $ \f -> do
    hSeek f AbsoluteSeek (fromIntegral (blocklen * blocknum))
    (Right cells,_) <- runGet getBlock <$> B.hGet f (fromIntegral blocklen)
    return (cells !! (fromIntegral cellnum))
    where blocklen = 196
          xblock = x `div` 8
          yblock = y `div` 8
          blocknum = (xblock * 512) + yblock
          cellnum = (x `mod` 8) + ((y `mod` 8) * 8)
          
getBlock :: Get [Int8]
getBlock = do
    skip 4 -- header, unknown content
    replicateM 64 getCell

getCell :: Get Int8
getCell = do
    --color <- getWord16le
    skip 2 -- dont use color...
    fromIntegral <$> getWord8
