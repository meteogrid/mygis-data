{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

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

import           MyGIS.Data.Error (EitherError, mapE, mkError)

class (Show d, Typeable d, Ord (DimIx d)) => Dimension d where
    type DimIx d :: *
    nextIx         :: d -> DimIx d -> DimIx d
    prevIx         :: d -> DimIx d -> DimIx d
    roundIx        :: d -> DimIx d -> DimIx d
    ceilIx         :: d -> DimIx d -> DimIx d
    floorIx        :: d -> DimIx d -> DimIx d
    enumFromToIx   :: d -> DimIx d -> DimIx d -> [DimIx d]

    enumFromToIx dim from to = worker start
        where worker i | i `cond` end = i : worker (adv i)
                       | otherwise    = []
              start = floorIx dim from
              end   = ceilIx dim to
              adv   = (if from < to then nextIx else prevIx) dim
              cond  = if from < to then (<=) else (>=)


data ObservationTimeDimension = ObservationTimeDimension CronSchedule
  deriving (Show, Typeable)

data ForecastTimeDimension = ForecastTimeDimension CronSchedule
                           | ForecastTimeDimensionF CronSchedule (Time->Horizons)
  deriving Typeable

instance Show ForecastTimeDimension where
  show (ForecastTimeDimension c) = "ForecastTimeDimension " ++ (show c)
  show (ForecastTimeDimensionF c _)
    = "ForecastTimeDimension " ++ (show c) ++ " <func>"

instance Dimension ObservationTimeDimension  where
    type DimIx ObservationTimeDimension = ObservationTimeIx
    nextIx       = undefined
    prevIx       = undefined
    ceilIx       = undefined
    floorIx      = undefined
    roundIx      = undefined

instance Dimension ForecastTimeDimension where
    type DimIx ForecastTimeDimension = ForecastTimeIx
    nextIx       = undefined
    prevIx       = undefined
    ceilIx       = undefined
    floorIx      = undefined
    roundIx      = undefined
 

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

mkHorizon :: (Integral a, Show a) => a -> EitherError Horizon
mkHorizon m | m >= 0     = Right . Minutes . fromIntegral $ m
            | otherwise  = mkError ("mkHorizon: '"
                                ++ (show m)
                                ++ "' Cannot be negative")

mkHorizons :: (Integral a, Show a) => [a] -> EitherError Horizons
mkHorizons [] = mkError "mkHorizons: Empty list"
mkHorizons xs = let result = mapE mkHorizon "; " xs in
                case result of
                    Right hs -> Right . sort . nub $ hs
                    e        -> e


mkSchedule :: Text -> EitherError CronSchedule
mkSchedule s = let result = parseOnly cronSchedule s in
               case result of
                 Right r -> Right r
                 Left e  -> mkError e
                
