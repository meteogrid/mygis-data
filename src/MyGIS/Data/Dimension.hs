module MyGIS.Data.Dimension (
    Dimension (..)
  , DimensionIx (..)
  , mkHorizon
  , mkHorizons
  , extractTime
  , secondsToNominalDiffTime
) where

import           Prelude hiding (min)
import           Data.List (nub, sort)
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Maybe (catMaybes)
import           System.Cron (CronSchedule)

newtype Horizon = Minutes {minutes :: Int}
  deriving (Eq, Ord, Show)

type Horizons = [Horizon]

mkHorizon :: Integral a => a -> Maybe Horizon
mkHorizon m | m >= 0     = Just . Minutes . fromIntegral $ m
            | otherwise  = Nothing

mkHorizons :: Integral a => [a] -> Maybe Horizons
mkHorizons hs = if length result == length hs
                then Just . sort . nub $ result
                else Nothing
  where result = catMaybes . map mkHorizon $ hs

data Dimension = TimeDim CronSchedule
               | ForecastTimeDim CronSchedule
               | HorizonDim Horizons
  deriving (Eq, Show)

data DimensionIx = TimeIx UTCTime
                 | FcTimeIx (UTCTime, Horizon)
  deriving (Eq, Show)


extractTime :: DimensionIx -> UTCTime
extractTime (TimeIx t) = t
extractTime (FcTimeIx (fct, h)) = addHorizonToTime h fct

addHorizonToTime :: Horizon -> UTCTime -> UTCTime
addHorizonToTime h t = addUTCTime (horizonToNominalDiffTime h) t

horizonToNominalDiffTime :: Horizon -> NominalDiffTime
horizonToNominalDiffTime = minutesToNominalDiffTime . fromIntegral . minutes

minutesToNominalDiffTime :: Integer -> NominalDiffTime
minutesToNominalDiffTime = secondsToNominalDiffTime . (*60)

secondsToNominalDiffTime :: Integer -> NominalDiffTime
secondsToNominalDiffTime s | s >= 0    = diffUTCTime b a
                           | otherwise = diffUTCTime a b
  where a = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
        b = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime $ abs s)

