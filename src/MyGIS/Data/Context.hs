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
  , width
  , height

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
    cid      :: !Text
  , envelope :: !Envelope
  , shape    :: !Shape
  , srs      :: !SpatialReference
} deriving (Eq, Show)

data Box a = Box {
    ll :: !a
  , ur :: !a
} deriving (Eq, Show)

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

{-# SPECIALIZE INLINE getX :: Point -> Double #-}
{-# SPECIALIZE INLINE getY :: Point -> Double #-}
{-# SPECIALIZE INLINE getX :: Pixel -> Int #-}
{-# SPECIALIZE INLINE getY :: Pixel -> Int #-}


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

{-# SPECIALIZE INLINE width :: Envelope -> Double #-}
{-# SPECIALIZE INLINE height :: Envelope -> Double #-}
{-# SPECIALIZE INLINE width :: Shape -> Int #-}
{-# SPECIALIZE INLINE height :: Shape -> Int #-}

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

{-# SPECIALIZE INLINE minx :: Envelope -> Double #-}
{-# SPECIALIZE INLINE miny :: Envelope -> Double #-}
{-# SPECIALIZE INLINE maxx :: Envelope -> Double #-}
{-# SPECIALIZE INLINE maxy :: Envelope -> Double #-}
{-# SPECIALIZE INLINE minx :: Shape -> Int #-}
{-# SPECIALIZE INLINE miny :: Shape -> Int #-}
{-# SPECIALIZE INLINE maxx :: Shape -> Int #-}
{-# SPECIALIZE INLINE maxy :: Shape -> Int #-}


forward :: Context -> Point -> Pixel
forward ctx (Point (x,y)) = Pixel (round ln, round col)
    where col = (x - (minx e)) * sx
          ln  = ((maxy e) - y) * sy
          e   = envelope ctx
          s   = shape ctx
          sx  = (fromIntegral (width s)) / width e
          sy  = (fromIntegral (height s)) / height e

backward :: Context -> Pixel -> Point
backward ctx (Pixel (ln,col)) = Point (x, y)
    where x = (minx e) + (fromIntegral col) / sx
          y = (maxy e) - (fromIntegral ln) / sy
          e   = envelope ctx
          s   = shape ctx
          sx  = (fromIntegral (width s)) / width e
          sy  = (fromIntegral (height s)) / height e

{-# INLINE forward #-}
{-# INLINE backward #-}
