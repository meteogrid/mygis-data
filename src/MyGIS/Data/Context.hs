{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module MyGIS.Data.Context (
    Context
  , Pixel (..)
  , Point (..)
  , Box
  , Envelope
  , Shape

  , isEmpty

  , mkBox
  , mkShape
  , mkEnvelope
  , mkContext

  , minx
  , miny
  , maxx
  , maxy
  , width
  , height

  , cid
  , srs
  , envelope
  , shape

  , forward
  , backward
  , forwardS
  , backwardS
  , intersection
  , intersects
  , union
) where

import           Control.Applicative ((<$>), (<*>))

import           Data.Data (Data, Typeable)
import           Data.Text (Text)
import           Data.Text.Binary()
import           Data.Monoid (Monoid(..))
import           Data.Binary (Binary(..))

import           MyGIS.Data.SpatialReference (SpatialReference)
import           MyGIS.Data.Error (mkError, EitherError)


data Context = Context {    
    cid      :: !Text
  , envelope :: !Envelope
  , shape    :: !Shape
  , srs      :: !SpatialReference
  } deriving (Eq, Show, Typeable, Data)

instance Binary Context where
  put (Context a b c d) = put a >> put b >> put c >> put d
  get                   = Context <$> get <*> get <*> get <*> get

mkContext ::
  Text -> Envelope -> Shape -> SpatialReference -> EitherError Context
mkContext i e s sr
    | isEmpty e || isEmpty s = mkError "mkContext: empty shape or envelope"
    | otherwise              = Right $ Context i e s sr

data Box a = Box {
    minx :: !a
  , miny :: !a
  , maxx :: !a
  , maxy :: !a
} deriving (Eq, Show, Typeable, Data)

instance (Ord a, Num a) => Monoid (Box a) where
    mempty = emptyBox
    mappend = union

instance (Binary a) => Binary (Box a) where
  put (Box a b c d) = put a >> put b >> put c >> put d
  get               = Box <$> get <*> get <*> get <*> get

emptyBox :: (Num a, Ord a) => Box a
emptyBox = Box 0 0 0 0

isEmpty :: (Num a, Ord a) => Box a -> Bool
isEmpty = (==) emptyBox

mkBox :: (Ord a, Num a)
  => a -> a -> a -> a ->  Box a
mkBox x0 y0 x1 y1
    | x1>x0 && y1>y0 = Box x0 y0 x1 y1
    | otherwise      = emptyBox

intersection :: (Ord a, Num a) => Box a -> Box a -> Box a
intersection a b
    | intersects a b = Box (max (minx a) (minx b))
                           (max (miny a) (miny b))
                           (min (maxx a) (maxx b))
                           (min (maxy a) (maxy b))
    | otherwise      = emptyBox

union :: (Ord a, Num a) => Box a -> Box a -> Box a
union a b
    | isEmpty a = b
    | isEmpty b = a
    | otherwise
        = Box (min (minx a) (minx b))
              (min (miny a) (miny b))
              (max (maxx a) (maxx b))
              (max (maxy a) (maxy b))

intersects :: (Ord a, Num a) => Box a -> Box a -> Bool
intersects a b = not ( (minx a > maxx b) ||
                       (maxx a < minx b) ||
                       (miny a > maxy b) ||
                       (maxy a < miny b) )
{-# INLINE intersects #-}


data Pixel = Pixel !Int !Int deriving (Eq, Show)
data SubPixel = SubPixel !Double !Double  deriving (Eq, Show)
data Point = Point !Double !Double  deriving (Eq, Show)

toSPixel :: Pixel -> SubPixel
toSPixel (Pixel x y) = SubPixel (fromIntegral x) (fromIntegral y)
{-# INLINE toSPixel #-}

fromSPixel :: SubPixel -> Pixel
fromSPixel (SubPixel x y) = Pixel (round x) (round y)
{-# INLINE fromSPixel #-}

width, height :: Num a => Box a -> a
width  b = (maxx b) - (minx b)
height b = (maxy b) - (miny b)

{-# SPECIALIZE INLINE width :: Envelope -> Double #-}
{-# SPECIALIZE INLINE height :: Envelope -> Double #-}
{-# SPECIALIZE INLINE width :: Shape -> Int #-}
{-# SPECIALIZE INLINE height :: Shape -> Int #-}

type Shape = Box Int

mkShape :: Int -> Int -> Shape
mkShape x y = mkBox 0 0 x y

type Envelope = Box Double

mkEnvelope :: Double -> Double -> Double -> Double ->  Envelope
mkEnvelope = mkBox


forwardS :: Context -> Point -> SubPixel
forwardS ctx (Point x y) = SubPixel ln col
    where col     = (x - (minx e)) * sx
          ln      = ((maxy e) - y) * sy
          (sx,sy) = scale ctx
          e        = (envelope ctx)

scale :: Context -> (Double, Double)
scale ctx = (sx,sy)
  where sx = (fromIntegral (width s)) / width e
        sy = (fromIntegral (height s)) / height e
        s  = shape ctx
        e  = envelope ctx
{-# INLINE scale #-}

backwardS :: Context -> SubPixel -> Point
backwardS ctx (SubPixel ln col) = Point x y
    where x       = (minx e) + col / sx
          y       = (maxy e) - ln / sy
          (sx,sy) = scale ctx
          e        = (envelope ctx)

forward :: Context -> Point -> Pixel
forward ctx = fromSPixel . (forwardS ctx)

backward :: Context -> Pixel -> Point
backward ctx = (backwardS ctx) . toSPixel

{-# INLINE forward #-}
{-# INLINE backward #-}
