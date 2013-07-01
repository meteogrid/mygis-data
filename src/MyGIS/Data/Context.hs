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

mkBox :: Pair a b
  => b -> b -> b -> b ->  EitherError (Box a)
mkBox x0 y0 x1 y1
    | x1>x0 && y1>y0 = Right $ Box (mkPair (x0,y0)) (mkPair (x1,y1))
    | otherwise      = mkError "mkBox: x1<=x0 or y1<=y0"

intersection :: Pair a b => Box a -> Box a -> Maybe (Box a)
intersection a b = case i of {Right r -> Just r; Left _ -> Nothing}
  where i    = mkBox
               (max (minx a) (minx b))
               (max (miny a) (miny b))
               (min (maxx a) (maxx b))
               (min (maxy a) (maxy b))

intersects :: Pair a b => Box a -> Box a -> Bool
intersects a = isJust . intersection a

class (Eq b, Show b, Num b, Ord b) => Pair a b | a->b where
    getX   :: a      -> b
    getY   :: a      -> b
    mkPair :: (b, b) -> a

{-# SPECIALIZE INLINE getX :: Point -> Double #-}
{-# SPECIALIZE INLINE getY :: Point -> Double #-}
{-# SPECIALIZE INLINE getX :: Pixel -> Int #-}
{-# SPECIALIZE INLINE getY :: Pixel -> Int #-}


data Pixel = Pixel !(Int,Int) deriving (Eq, Show)
data Point = Point !(Double,Double) deriving (Eq, Show)

instance Pair Pixel Int where
    getX !(Pixel (x,_)) = x
    getY !(Pixel (_,y)) = y
    mkPair = Pixel

instance Pair Point Double where
    getX !(Point (x,_)) = x
    getY !(Point (_,y)) = y
    mkPair = Point

-- definimos la altura y ancho de una caja
width, height :: Pair a b => Box a -> b
width  !b = (maxx b) - (minx b)
height !b = (maxy b) - (miny b)

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

minx, maxx, miny, maxy :: Pair a b => Box a -> b
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
