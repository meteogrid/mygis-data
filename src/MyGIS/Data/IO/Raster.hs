module MyGIS.Data.IO.Raster (
    Raster (..)
  , Options (..)
  , Window

  , readWindow
  , generatePixel

) where

import qualified Data.Vector.Storable as V
import MyGIS.Data.Context (Context, Shape, Pixel, Point)

data Options = Opts {
    path        :: FilePath
  , compression :: Maybe Int
} deriving (Eq, Show)

data Raster a = Raster Options Context [Block a]

data Block a = Block Shape (V.Vector a)

data Window a = Window Context (V.Vector a)

readWindow :: Raster a -> Context -> IO (Window a)
readWindow = undefined

generatePixel :: Options -> Context -> (Pixel -> a) -> IO (Raster a)
generatePixel = undefined

generatePoint :: Options -> Context -> (Point -> a) -> IO (Raster a)
generatePoint = undefined

