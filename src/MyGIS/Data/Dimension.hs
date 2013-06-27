{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module MyGIS.Data.Dimension (
    Dimension (..)
  , ObservationTimeDimension (..) 
  , ForecastTimeDimension (..) 
  , ObservationTimeIx (..) 
  , ForecastTimeIx (..) 
  , mkHorizon
  , mkSchedule
  , mkHorizons
  , mkTime
) where

import           Data.List (nub, sort)
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Data.Time.Clock
import           Data.Time.Calendar
import           System.Cron (CronSchedule)
import           System.Cron.Parser (cronSchedule)
import           Data.Attoparsec.Text (parseOnly)

import           MyGIS.Data.Error (EitherError, mapE)

class (Eq d, Show d, Typeable d) => Dimension d where
    type DimIx d :: *
    next         :: d -> DimIx d -> DimIx d
    prev         :: d -> DimIx d -> DimIx d
    round        :: d -> DimIx d -> DimIx d
    ceil         :: d -> DimIx d -> DimIx d
    floor        :: d -> DimIx d -> DimIx d
    listFrom     :: d -> DimIx d -> [DimIx d]
    listFromTo   :: d -> DimIx d -> [DimIx d]


data ObservationTimeDimension = ObservationTimeDimension CronSchedule
  deriving (Eq, Show, Typeable)

data ForecastTimeDimension = ForecastTimeDimension CronSchedule Horizons
  deriving (Eq, Show, Typeable)


instance Dimension ObservationTimeDimension  where
    type DimIx ObservationTimeDimension = ObservationTimeIx
    next = undefined
    prev = undefined
    ceil       = undefined
    floor      = undefined
    round      = undefined
    listFrom   = undefined
    listFromTo = undefined

instance Dimension ForecastTimeDimension where
    type DimIx ForecastTimeDimension = ForecastTimeIx
    next = undefined
    prev = undefined
    ceil = undefined
    floor = undefined
    round = undefined
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

mkHorizon :: Integral a => a -> EitherError Horizon
mkHorizon m | m >= 0     = Right . Minutes . fromIntegral $ m
            | otherwise  = Left "mkHorizon: Cannot be negative"

mkHorizons :: Integral a => [a] -> EitherError Horizons
mkHorizons [] = Left "mkHorizons: Empty list"
mkHorizons xs = let result = mapE mkHorizon "; " xs
                in case result of
                        Right hs -> Right . sort . nub $ hs
                        e        -> e


mkSchedule :: Text -> Either String CronSchedule
mkSchedule = parseOnly cronSchedule
