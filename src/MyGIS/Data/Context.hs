{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module MyGIS.Data.Context (
    Context (..)
  , Pixel (..)
  , Point (..)
  , Envelope
  , Shape

  , mkShape
  , mkEnvelope

  , forward
  , backward
) where

import           Data.Text (Text)

import           MyGIS.Data.SpatialReference (SpatialReference)
import           MyGIS.Data.Error (mkError, EitherError)


data Context = Context {
                    cid   :: !Text
                  , box   :: !Envelope
                  , shape :: !Shape
                  , srs   :: !SpatialReference
} deriving (Eq, Show)

data Box a = Box {ll :: !a, ur :: !a} deriving (Eq, Show)

class (Eq (PairType a), Show (PairType a), Num (PairType a)) => Pair a where
    type PairType a :: *
    getX :: a -> PairType a
    getY :: a -> PairType a

newtype Pixel = Pixel (Int,Int) deriving (Eq, Show)
newtype Point = Point (Double,Double) deriving (Eq, Show)

instance Pair Pixel where
    type PairType Pixel = Int
    getX (Pixel (x,_)) = x
    getY (Pixel (_,y)) = y

instance Pair Point where
    type PairType Point = Double
    getX (Point (x,_)) = x
    getY (Point (_,y)) = y

-- definimos la altura y ancho de una caja
width, height :: Pair a => Box a -> PairType a
width  b = (maxx b) - (minx b)
height b = (maxy b) - (miny b)


type Shape = Box Pixel

mkShape :: Int -> Int -> EitherError Shape
mkShape x y | x>0 && y>0 = Right $ Box (Pixel (0,0)) (Pixel (x,y))
            | otherwise  =  mkError "mkShape: x and y must be both > 0"

type Envelope = Box Point

mkEnvelope :: Double -> Double -> Double -> Double ->  EitherError Envelope
mkEnvelope x0 y0 x1 y1
    | x1>x0 && y1>y0 = Right $ Box (Point (x0,y0)) (Point (x1,y1))
    | otherwise      = mkError "mkEnvelope: x1<=x0 or y1<=y0"

minx, maxx, miny, maxy :: Pair a => Box a -> PairType a
minx = getX . ll
maxx = getX . ur
miny = getY . ll
maxy = getY . ur



forward :: Context -> Point -> Pixel
forward ctx = point2pixel (geotransform ctx)

backward :: Context -> Pixel -> Point
backward ctx = pixel2point (geotransform ctx)

-- Tipo para la matriz de transformacion
data GeoTransform = GeoTransform !Double !Double !Double !Double !Double !Double
    deriving (Eq, Show)


-- para construir una matriz de transformacion a partir de
-- un bbox y forma del raster
mkGeoTransform :: Envelope -> Shape -> GeoTransform
mkGeoTransform b s =
    GeoTransform (minx b) dx 0 (maxy b) 0 (-dy)
    where dx = width b  / fromIntegral (width s)
          dy = height b / fromIntegral (height s)

geotransform :: Context -> GeoTransform
geotransform c = mkGeoTransform (box c) (shape c)


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
