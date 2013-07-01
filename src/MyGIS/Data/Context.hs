{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module MyGIS.Data.Context (
    Context (..)
  , Pixel (..)
  , Point (..)
  , Box
  , Envelope
  , Shape

  , mkBox
  , mkShape
  , mkEnvelope

  , minx
  , miny
  , maxx
  , maxy

  , forward
  , backward
  , intersection
  , intersects
) where

import           Data.Text (Text)
import           Data.Maybe (isJust)

import           MyGIS.Data.SpatialReference (SpatialReference)
import           MyGIS.Data.Error (mkError, EitherError)


data Context = Context {
                    cid   :: !Text
                  , box   :: !Envelope
                  , shape :: !Shape
                  , srs   :: !SpatialReference
} deriving (Eq, Show)

data Box a = Box {ll :: !a, ur :: !a} deriving (Eq, Show)

mkBox :: Pair a
  => PairType a
  -> PairType a
  -> PairType a
  -> PairType a
  ->  EitherError (Box a)
mkBox x0 y0 x1 y1
    | x1>x0 && y1>y0 = Right $ Box (mkPair x0 y0) (mkPair x1 y1)
    | otherwise      = mkError "mkBox: x1<=x0 or y1<=y0"

intersection :: Pair a => Box a -> Box a -> Maybe (Box a)
intersection a b = case i of {Right r -> Just r; Left _ -> Nothing}
  where i    = mkBox (pj minx) (pj miny) (pj maxx) (pj maxy)
        pj f = (f a `min` f b) + abs (f a - f b)

intersects :: Pair a => Box a -> Box a -> Bool
intersects a = isJust . intersection a

class (Eq (PairType a), Show (PairType a), Num (PairType a), Ord (PairType a))
  => Pair a where
    type PairType a :: *
    getX   :: a -> PairType a
    getY   :: a -> PairType a
    mkPair :: PairType a -> PairType a -> a


newtype Pixel = Pixel (Int,Int) deriving (Eq, Show)
newtype Point = Point (Double,Double) deriving (Eq, Show)

instance Pair Pixel where
    type PairType Pixel = Int
    getX (Pixel (x,_)) = x
    getY (Pixel (_,y)) = y
    mkPair = curry Pixel

instance Pair Point where
    type PairType Point = Double
    getX (Point (x,_)) = x
    getY (Point (_,y)) = y
    mkPair = curry Point

-- definimos la altura y ancho de una caja
width, height :: Pair a => Box a -> PairType a
width  b = (maxx b) - (minx b)
height b = (maxy b) - (miny b)


type Shape = Box Pixel

mkShape :: Int -> Int -> EitherError Shape
mkShape x y = mkBox 0 0 x y

type Envelope = Box Point

mkEnvelope :: Double -> Double -> Double -> Double ->  EitherError Envelope
mkEnvelope = mkBox

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
