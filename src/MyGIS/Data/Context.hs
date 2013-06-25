{-# LANGUAGE BangPatterns #-}

module MyGIS.Data.Context (
    Context (..)
  , Box (..)
  , Shape (..)
  , forward
  , backward
) where

import           Data.Text (Text)

import           MyGIS.Data.SpatialReference (SpatialReference)


data Context = Context {
                    cid   :: !Text
                  , box   :: !Box
                  , shape :: !Shape
                  , srs   :: !SpatialReference
} deriving (Eq, Show)

data Box = Box {
             x0   :: !Double   
           , y0   :: !Double   
           , x1   :: !Double   
           , y1   :: !Double   
} deriving (Eq, Show)

data Shape = Shape {
               nx  :: !Int 
             , ny  :: !Int
} deriving (Eq, Show)


forward :: Context -> Point -> Pixel
forward ctx = point2pixel (geotransform ctx)

backward :: Context -> Pixel -> Point
backward ctx = pixel2point (geotransform ctx)

-- Tipo para la matriz de transformacion
data GeoTransform = GeoTransform !Double !Double !Double !Double !Double !Double
    deriving (Eq, Show)

-- definimos la altura y ancho de una caja
width, height :: Box -> Double
width  b = (x1 b) - (x0 b)
height b = (y1 b) - (y0 b)

-- para construir una matriz de transformacion a partir de
-- un bbox y forma del raster
mkGeoTransform :: Box -> Shape -> GeoTransform
mkGeoTransform b s =
    GeoTransform (x0 b) dx 0 (y1 b) 0 (-dy)
    where dx = width b / fromIntegral (nx s)
          dy = height b / fromIntegral (ny s)

geotransform :: Context -> GeoTransform
geotransform c = mkGeoTransform (box c) (shape c)

newtype Pixel = Pixel (Int, Int) deriving (Eq, Show)
newtype Point = Point (Double, Double) deriving (Eq, Show)

pixel2point :: GeoTransform -> Pixel -> Point
pixel2point (GeoTransform gt0 gt1 gt2 gt3 gt4 gt5) (Pixel (ln, col))
    = Point (x, y)
  where x    = gt0 + col'*gt1 + ln'*gt2
        y    = gt3 + col'*gt4 + ln'*gt5
        col' = fromIntegral col
        ln'  = fromIntegral ln
    
point2pixel :: GeoTransform -> Point -> Pixel
point2pixel (GeoTransform gt0 gt1 gt2 gt3 gt4 gt5) (Point (x, y))
    = Pixel (floor ln, floor col)
  where ln     = detA1B  / detA
        col    = detA2B  / detA
        detA   = gt1*gt5 - gt2*gt4
        detA1B = gt1*y'   + gt4*x'
        detA2B = gt2*y'   + gt5*x'
        x'     = x - gt0
        y'     = y - gt3
