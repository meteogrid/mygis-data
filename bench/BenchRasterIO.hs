{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main hiding (defaultOptions)
import System.IO
import System.IO.Temp
import System.FilePath

import MyGIS.Data
import MyGIS.Data.IO

main :: IO ()
main = withSystemTempDirectory "bench." $ \tmpDir -> do
  let tP f       = joinPath [tmpDir, f]
      Right c    = mkContext "" (mkEnvelope 0 0 1 1) (mkShape 3000 3000) ""
      rs         = [ Raster defaultOptions {compression=i} c (tP ("r"++show i)) |
                     i <- [0..9] ] 
      pFunc (Pixel i j)
                 = fromIntegral $ (i `mod` 8) * (j `mod` 8)
      writeActs  = [runSession $ (try . (pixelGenerator pFunc r)) >-> writerS r |
                    r <-rs ]
      readActs   = [runSession (readerS r  >-> try . (sink r)) | r <-rs ]
          
  defaultMain (
    [ bench ("Writing comp level: "  ++ show i) (whnfIO act) |
      (i,act) <- zip [0..] writeActs]
      ++
    [ bench ("Reading comp level: "  ++ show i) (whnfIO act) |
      (i,act) <- zip [0..] readActs]
    )
    
