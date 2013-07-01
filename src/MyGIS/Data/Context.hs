{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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
  , width
  , height

  , forward
  , backward
  , forwardS
  , backwardS
  , intersection
  , intersects
) where

import           Data.Text (Text)
import           Data.Maybe (isJust)

import           MyGIS.Data.SpatialReference (SpatialReference)
import           MyGIS.Data.Error (mkError, EitherError)


data Context = Context {
    cid      :: !Text
  , envelope :: !Envelope
  , shape    :: !Shape
  , srs      :: !SpatialReference
} deriving (Eq, Show)


data Box a = Box {
    minx :: !a
  , miny :: !a
  , maxx :: !a
  , maxy :: !a
} deriving (Eq, Show)

mkBox :: Ord a
  => a -> a -> a -> a ->  EitherError (Box a)
mkBox x0 y0 x1 y1
    | x1>x0 && y1>y0 = Right $ Box x0 y0 x1 y1
    | otherwise      = mkError "mkBox: x1<=x0 or y1<=y0"

intersection :: Ord a => Box a -> Box a -> Maybe (Box a)
intersection a b = case i of {Right r -> Just r; Left _ -> Nothing}
  where i    = mkBox
               (max (minx a) (minx b))
               (max (miny a) (miny b))
               (min (maxx a) (maxx b))
               (min (maxy a) (maxy b))

intersects :: Ord a => Box a -> Box a -> Bool
intersects a = isJust . intersection a

data Pixel = Pixel !Int !Int deriving (Eq, Show)
data Point = Point !Double !Double  deriving (Eq, Show)
data SubPixel = SubPixel !Double !Double  deriving (Eq, Show)

toSPixel :: Pixel -> SubPixel
toSPixel (Pixel x y) = SubPixel (fromIntegral x) (fromIntegral y)

fromSPixel :: SubPixel -> Pixel
fromSPixel (SubPixel x y) = Pixel (round x) (round y)

-- definimos la altura y ancho de una caja
width, height :: Num a => Box a -> a
width  !b = (maxx b) - (minx b)
height !b = (maxy b) - (miny b)

{-# SPECIALIZE INLINE width :: Envelope -> Double #-}
{-# SPECIALIZE INLINE height :: Envelope -> Double #-}
{-# SPECIALIZE INLINE width :: Shape -> Int #-}
{-# SPECIALIZE INLINE height :: Shape -> Int #-}

type Shape = Box Int

mkShape :: Int -> Int -> EitherError Shape
mkShape x y = mkBox 0 0 x y

type Envelope = Box Double

mkEnvelope :: Double -> Double -> Double -> Double ->  EitherError Envelope
mkEnvelope = mkBox


forwardS :: Context -> Point -> SubPixel
forwardS ctx (Point x y) = SubPixel ln col
    where col = (x - (minx e)) * sx
          ln  = ((maxy e) - y) * sy
          e   = envelope ctx
          s   = shape ctx
          sx  = (fromIntegral (width s)) / width e
          sy  = (fromIntegral (height s)) / height e

backwardS :: Context -> SubPixel -> Point
backwardS ctx (SubPixel ln col) = Point x y
    where x  = (minx e) + col / sx
          y  = (maxy e) - ln / sy
          e  = envelope ctx
          s  = shape ctx
          sx = (fromIntegral (width s)) / width e
          sy = (fromIntegral (height s)) / height e

forward :: Context -> Point -> Pixel
forward ctx = fromSPixel . (forwardS ctx)

backward :: Context -> Pixel -> Point
backward ctx = (backwardS ctx) . toSPixel

{-# INLINE forward #-}
{-# INLINE backward #-}
