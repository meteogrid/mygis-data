{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module MyGIS.Data.Dimension (
    Dimension (..)
  , DimensionIx (..)
  , DimensionIxKey
  , ObservationTimeDimension (..) 
  , ForecastTimeDimension (..) 
  , ObservationTimeIx (..) 
  , ForecastTimeIx (..) 
) where

import           Data.Text (Text, pack)
import           Data.Typeable (Typeable)

import MyGIS.Data.Time (CronSchedule, Horizon, Time)


class (Show d, Typeable d, DimensionIx (DimIx d)) => Dimension d where
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



data ForecastTimeDimension =
    ForecastTimeDimension CronSchedule
  | ForecastTimeDimensionF CronSchedule (Time->[Horizon])
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
 

type DimensionIxKey = [Text]

class (Show ix, Typeable ix, Ord ix, Eq ix) => DimensionIx ix where
    ixKey          :: ix -> DimensionIxKey



data ObservationTimeIx = ObservationTimeIx Time
  deriving (Eq, Ord, Show, Typeable)

instance DimensionIx ObservationTimeIx where
    ixKey (ObservationTimeIx t) = map pack [show t]

data ForecastTimeIx = ForecastTimeIx Time Horizon
  deriving (Eq, Ord, Show, Typeable)

instance DimensionIx ForecastTimeIx where
    ixKey (ForecastTimeIx t h) = map pack [show t, show h]
