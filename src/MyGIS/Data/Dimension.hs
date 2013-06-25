module MyGIS.Data.Dimension (
    Dimension (..)
  , DimensionIx (..)
  , mkHorizon
  , mkHorizons
  , mkTime
  , extractTime
  , secondsToNominalDiffTime
) where

import           Data.List (nub, sort, intersperse)
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Either (partitionEithers)
import           System.Cron (CronSchedule)

newtype Horizon = Minutes {minutes :: Int}
  deriving (Eq, Ord, Show)

type Horizons = [Horizon]

type Error = String

mkHorizon :: Integral a => a -> Either Error Horizon
mkHorizon m | m >= 0     = Right . Minutes . fromIntegral $ m
            | otherwise  = Left "mkHorizon: Cannot be negative"

mkHorizons :: Integral a => [a] -> Either Error Horizons
mkHorizons [] = Left "mkHorizons: Empty list"
mkHorizons hs = if errors /= []
                then Right . sort . nub $ result
                else Left . concat . intersperse "; " $ errors
  where (errors,result) = partitionEithers . map mkHorizon $ hs

data Dimension = NoDimension
               | TimeDimension
  deriving (Eq, Show)
    
data TimeDimension = ObservedTime CronSchedule
                   | ForecastedTime CronSchedule Horizons
  deriving (Eq, Show)

data DimensionIx = NoDimensionIx
                 | TimeDimensionIx
  deriving (Eq, Show)

newtype Time = Time {getTime :: UTCTime} deriving (Eq, Ord, Show)

mkTime :: UTCTime -> Time
mkTime = Time

data TimeDimensionIx = ObservedTimeIx Time
                     | ForecastedTimeIx Time Horizon

extractTime :: TimeDimensionIx -> Time
extractTime (ObservedTimeIx   t)        = t
extractTime (ForecastedTimeIx t h) = addHorizonToTime t h

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

