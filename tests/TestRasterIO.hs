{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TestRasterIO (tests) where

import System.IO
import System.IO.Temp
import System.FilePath
import qualified Data.ByteString.Lazy as L
import Control.Exception hiding (try)

import Test.QuickCheck
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

import MyGIS.Data
import MyGIS.Data.IO


tests = $(testGroupGenerator)



case_reader_and_writer_can_duplicate_file = do
    let pFunc (Pixel i j)
            = fromIntegral (i*j)
        Right ctx
            = mkContext "" (mkEnvelope 0 0 100 100) (mkShape 1000 1000) ""
    withSystemTempDirectory "test." $ \tmpDir -> do
        let path1   = joinPath [tmpDir, "prueba1.bin"]
            path2   = joinPath [tmpDir, "prueba2.bin"]
            raster  = Raster (Opts Nothing (256,256)) ctx path1
            raster2 = Raster (Opts Nothing (256,256)) ctx path2

        runSession $
            (try . (pixelGenerator pFunc raster)) >-> writerS raster

        runSession $
            readerS raster >-> writerS raster2

        Just size <- getFileSize path1
        assertBool "File too small" $ size > 1000*1000*2

        assertFilesEqual path1 path2
        


assertFilesEqual :: FilePath -> FilePath -> IO ()
assertFilesEqual a b = do
    f <- openFile a ReadMode
    hSetBinaryMode f True
    f2 <- openFile b ReadMode
    hSetBinaryMode f2 True
    c <- L.hGetContents f
    c2 <- L.hGetContents f2
    let !ret = all id $ L.zipWith (==) c c2
    hClose f
    hClose f2
    assertBool (show a ++ " is not equal to " ++ show b) ret
    return ()

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler $ bracket
    (openFile path ReadMode)
    (hClose)
    (\h -> do {size <- hFileSize h; return $ Just size})
  where
    handler :: SomeException -> IO (Maybe Integer)
    handler _ = return Nothing
