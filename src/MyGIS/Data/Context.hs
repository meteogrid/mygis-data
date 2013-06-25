{-# LANGUAGE BangPatterns #-}

module MyGIS.Data.Context (
    Context (..)
  , Box (..)
  , Shape (..)
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



-- Tipo para la matriz de transformacion
data GeoTransform = GeoTransform !Double !Double !Double !Double !Double !Double
    deriving (Eq, Show)

-- definimos la altura y ancho de una caja
width, height :: Box -> Double
width  (Box x0  _ x1  _) = x1 - x0
height (Box  _ y0  _ y1) = y1 - y0

-- para construir una matriz de transformacion a partir de
-- un bbox y forma del raster
mkGeoTransform :: Box -> Shape -> GeoTransform
mkGeoTransform b@(Box x0 _ _ y1) (Shape nx ny) =
    GeoTransform x0 dx 0 y1 0 (-dy)
    where dx = width b / fromIntegral nx
          dy = height b / fromIntegral ny

geotransform :: Context -> GeoTransform
geotransform c = mkGeoTransform (box c) (shape c)

type Pixel = (Int, Int)
type Point = (Double, Double)

pixel2point :: GeoTransform -> Pixel -> Point
pixel2point (GeoTransform gt0 gt1 gt2 gt3 gt4 gt5) (ln,col) = (px,py)
    where px   = gt0 + col'*gt1 + ln'*gt2
          py   = gt3 + col'*gt4 + ln'*gt5
          col' = fromIntegral col
          ln'  = fromIntegral ln
    
point2pixel :: GeoTransform -> Point -> Pixel
point2pixel (GeoTransform gt0 gt1 gt2 gt3 gt4 gt5) (x,y) = (floor ln, floor col)
    where ln     = detA1B  / detA
          col    = detA2B  / detA
          detA   = gt1*gt5 - gt2*gt4
          x0     = x - gt0
          y0     = y - gt3
          detA1B = gt1*y0   + gt4*x0
          detA2B = gt2*y0   + gt5*x0
