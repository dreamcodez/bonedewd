{-# OPTIONS_GHC -Wall #-}
module BoneDewd.MapParse (getElevation) where
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Get
import Data.Int
import Data.Word
import System.FilePath ((</>))
import System.IO (IOMode(..), SeekMode(..), hSeek, withBinaryFile)

getElevation :: (Word16,Word16) -> IO Int8
getElevation (x,y) = do
    withBinaryFile ("mul" </> "map0.mul") ReadMode $ \f -> do
    hSeek f AbsoluteSeek (fromIntegral seekLoc)
    runGet (get :: Get Int8) <$> L.hGet f 1
    where blocklen = 196 :: Int
          xblock = (fromIntegral x) `div` 8 :: Int
          yblock = (fromIntegral y) `div` 8 :: Int
          blocknum = (xblock * 512) + yblock :: Int
          cellLen = 3 :: Int
          xcell = (fromIntegral x) `mod` 8 :: Int
          ycell = (fromIntegral y) `mod` 8 :: Int
          cellnum = xcell + (ycell * 8) :: Int
          seekLoc = (blocklen * blocknum) + 4 + (cellLen * cellnum) + 2 :: Int
