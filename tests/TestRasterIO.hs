{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestRasterIO (tests) where

import Data.Int
import Data.Maybe
import System.IO
import System.IO.Temp
import System.FilePath
import qualified Data.ByteString.Lazy as L
import Control.Exception hiding (try)
import Control.Applicative
import Control.Monad

import Test.Framework
import Test.Framework.TH
-- import Test.QuickCheck
-- import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

import MyGIS.Data
import MyGIS.Data.IO


tests :: Test.Framework.Test
tests = $(testGroupGenerator)

case_reader_and_writer_can_duplicate_raster :: IO ()
case_reader_and_writer_can_duplicate_raster
    = write_read_write_and_verify 0

case_reader_and_writer_can_duplicate_compressed_raster :: IO ()
case_reader_and_writer_can_duplicate_compressed_raster
    = write_read_write_and_verify 9

write_read_write_and_verify :: Int ->  IO()
write_read_write_and_verify comp = do
    let pFunc (Pixel i j)
            = fromIntegral (i*j)
        Right ctx
            = mkGeoReference (mkExtent 0 0 100 100) (mkShape 1000 1000) ""
    withSystemTempDirectory "test." $ \tmpDir -> do
        let path1   = joinPath [tmpDir, "prueba1.bin"]
            path2   = joinPath [tmpDir, "prueba2.bin"]
            raster, raster2 :: Raster Int16
            raster  = Raster defaultOptions {compression = comp} ctx path1
            raster2 = Raster defaultOptions {compression = comp} ctx path2

        runSession $
            (try . (pixelGenerator pFunc raster)) >-> writerS raster

        runSession $
            readerS raster >-> writerS raster2

        assertExistsAndSizeGreaterThan path1 (1000*1000*2)
        assertExistsAndSizeGreaterThan path2 (1000*1000*2)
        assertFilesEqual path1 path2
        
case_compress_makes_smaller_files_for_all_same_value ::  IO()
case_compress_makes_smaller_files_for_all_same_value = do
    let Right ctx
            = mkGeoReference (mkExtent 0 0 100 100) (mkShape 1000 1000) ""
    withSystemTempDirectory "test." $ \tmpDir -> do
        let path1   = joinPath [tmpDir, "uncompressed.bin"]
            path2   = joinPath [tmpDir, "compressed.bin"]
            opts1   = defaultOptions {compression = 0}
            opts2   = defaultOptions {compression = 9}
            raster, raster2 :: Raster Int16
            raster  = Raster opts1 ctx path1
            raster2 = Raster opts2 ctx path2

        runSession $
            (try . (replicateGenerator 0 raster)) >-> writerS raster

        runSession $
            readerS raster >-> writerS raster2

        assertExistsAndSizeGreaterThan path2 1
        compressedSize <- liftM fromJust $ getFileSize path2
        assertExistsAndSizeGreaterThan path1 compressedSize

assertExistsAndSizeGreaterThan :: FilePath -> Integer -> IO ()
assertExistsAndSizeGreaterThan p s = do
    size <- getFileSize p
    assertBool ("file " ++ show p ++ " was not created") (isJust size)
    let s2 = fromJust size
    assertBool ("File size of " ++ show p ++ ", " ++ show s2 ++ " is <= " ++
                show s)
               (s2 > s)
    

assertFilesEqual :: FilePath -> FilePath -> IO ()
assertFilesEqual a b =
    withBinaryFile a ReadMode $ \f ->
        withBinaryFile b ReadMode $ \f2 -> do
            contents <- liftM2 L.zip (L.hGetContents f) (L.hGetContents f2)
            assertBool
                (show a ++ " is not equal to " ++ show b)
                (all (uncurry (==)) contents)


getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize p
    = handle (\(_ :: SomeException) -> return Nothing) $
        withBinaryFile p ReadMode $ \h -> Just <$> hFileSize h
