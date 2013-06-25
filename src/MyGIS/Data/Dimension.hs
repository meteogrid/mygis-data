{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MyGIS.Data.Dimension (
    Dimension (..)
  , ObservationTimeDimension (..) 
  , ForecastTimeDimension (..) 
  , ObservationTimeIx (..) 
  , ForecastTimeIx (..) 
  , mkHorizon
  , mkHorizons
  , mkTime
) where

import           Data.List (nub, sort, intersperse)
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Either (partitionEithers)
import           System.Cron (CronSchedule)

class (Eq d, Show d, Eq ix, Show ix, Ord ix) => Dimension d ix | d->ix where
    next         :: d -> ix -> ix
    prev         :: d -> ix -> ix
    roundUp      :: d -> ix -> ix
    roundDown    :: d -> ix -> ix
    listFrom     :: d -> ix -> [ix]
    listFromTo   :: d -> ix -> [ix]


data ObservationTimeDimension = ObservationTimeDimension CronSchedule
  deriving (Eq, Show)

data ForecastTimeDimension = ForecastTimeDimension CronSchedule Horizons
  deriving (Eq, Show)


instance Dimension ObservationTimeDimension ObservationTimeIx where
    next = undefined
    prev = undefined
    roundUp = undefined
    roundDown = undefined
    listFrom = undefined
    listFromTo = undefined

instance Dimension ForecastTimeDimension ForecastTimeIx where
    next = undefined
    prev = undefined
    roundUp = undefined
    roundDown = undefined
    listFrom = undefined
    listFromTo = undefined

 

data ObservationTimeIx = ObservationTimeIx Time
  deriving (Eq, Ord, Show)

data ForecastTimeIx = ForecastTimeIx Time Horizon
  deriving (Eq, Ord, Show)



newtype Time = Time {getTime :: UTCTime} deriving (Eq, Ord, Show)

mkTime :: UTCTime -> Time
mkTime = Time


addHorizonToTime :: Time -> Horizon -> Time
addHorizonToTime t h
    = Time $ addUTCTime (horizonToNominalDiffTime h) (getTime t)

horizonToNominalDiffTime :: Horizon -> NominalDiffTime
horizonToNominalDiffTime = minutesToNominalDiffTime . fromIntegral . minutes

minutesToNominalDiffTime :: Integer -> NominalDiffTime
minutesToNominalDiffTime = secondsToNominalDiffTime . (*60)

secondsToNominalDiffTime :: Integer -> NominalDiffTime
secondsToNominalDiffTime s | s >= 0    = diffUTCTime b a
                           | otherwise = diffUTCTime a b
  where a = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
        b = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime $ abs s)


newtype Horizon = Minutes {minutes :: Int}
  deriving (Eq, Ord, Show)

type Horizons = [Horizon]

type Error = String

mkHorizon :: Integral a => a -> Either Error Horizon
mkHorizon m | m >= 0     = Right . Minutes . fromIntegral $ m
            | otherwise  = Left "mkHorizon: Cannot be negative"

mkHorizons :: Integral a => [a] -> Either Error Horizons
mkHorizons [] = Left "mkHorizons: Empty list"
mkHorizons hs = if null errors
                then Right . sort . nub $ result
                else Left . concat . intersperse "; " $ errors
  where (errors,result) = partitionEithers . map mkHorizon $ hs
