{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module MyGIS.Data.Dimension (
    IsDimension (..)
  , IsDimensionIx (..)
  , IsDimensionIxKey
  , ObservationTimeDimension (..) 
  , ForecastTimeDimension (..) 
  , ObservationTimeIx (..) 
  , ForecastTimeIx (..) 
) where

import           Data.Data (Data, Typeable)
import           Data.Text hiding (map)

import MyGIS.Data.Time
import MyGIS.Data.ThirdPartyInstances()


class (Show d, Data d, Eq d, IsDimensionIx (DimIx d)) => IsDimension d where
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
  deriving (Show, Eq, Typeable, Data)



data ForecastTimeDimension = ForecastTimeDimension CronSchedule [Horizon]
  deriving (Show, Eq, Typeable, Data)


instance IsDimension ObservationTimeDimension  where
    type DimIx ObservationTimeDimension = ObservationTimeIx
    nextIx       = undefined
    prevIx       = undefined
    ceilIx       = undefined
    floorIx      = undefined
    roundIx      = undefined

instance IsDimension ForecastTimeDimension where
    type DimIx ForecastTimeDimension = ForecastTimeIx
    nextIx       = undefined
    prevIx       = undefined
    ceilIx       = undefined
    floorIx      = undefined
    roundIx      = undefined
 

type IsDimensionIxKey = [Text]

class (Show ix, Data ix, Ord ix, Eq ix) => IsDimensionIx ix where
    ixKey          :: ix -> IsDimensionIxKey



data ObservationTimeIx = ObservationTimeIx Time
  deriving (Eq, Ord, Show, Typeable, Data)

instance IsDimensionIx ObservationTimeIx where
    ixKey (ObservationTimeIx t) = map pack [show t]

data ForecastTimeIx = ForecastTimeIx Time Horizon
  deriving (Eq, Ord, Show, Typeable, Data)

instance IsDimensionIx ForecastTimeIx where
    ixKey (ForecastTimeIx t h) = map pack [show t, show h]
