{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SIGyM.Dimension (
    IsDimension (..)
  , NullDimension (..)
  , ObservationTimeDimension (..) 
  , ForecastTimeDimension (..) 
  , NullIx (..)
  , ObservationTimeIx (..) 
  , ForecastTimeIx (..) 
) where

import           Data.Typeable (Typeable)

import SIGyM.Time
import SIGyM.ThirdPartyInstances()


class ( Show d, Typeable d, Eq d
      , Show (DimIx d), Eq (DimIx d), Ord (DimIx d), Typeable (DimIx d) )
  => IsDimension d where
    type DimIx d :: *
    nextIx         :: d -> DimIx d -> DimIx d
    prevIx         :: d -> DimIx d -> DimIx d
    roundIx        :: d -> DimIx d -> DimIx d
    ceilIx         :: d -> DimIx d -> DimIx d
    floorIx        :: d -> DimIx d -> DimIx d
    enumFromToIx   :: d -> DimIx d -> DimIx d -> [DimIx d]

    nextIx       = undefined
    prevIx       = undefined
    ceilIx       = undefined
    floorIx      = undefined
    roundIx      = undefined

    enumFromToIx dim from to = worker start
        where worker i | i `cond` end = i : worker (adv i)
                       | otherwise    = []
              start = floorIx dim from
              end   = ceilIx dim to
              adv   = (if from < to then nextIx else prevIx) dim
              cond  = if from < to then (<=) else (>=)

data NullDimension = NullDimension deriving (Eq, Show, Typeable)

data NullIx = NullIx deriving (Eq, Ord, Show, Typeable)

instance IsDimension NullDimension  where
    type DimIx NullDimension = NullIx


data ObservationTimeDimension = ObservationTimeDimension CronSchedule
  deriving (Show, Eq, Typeable)

data ObservationTimeIx = ObservationTimeIx Time
  deriving (Eq, Ord, Show, Typeable)

instance IsDimension ObservationTimeDimension  where
    type DimIx ObservationTimeDimension = ObservationTimeIx



data ForecastTimeDimension = ForecastTimeDimension CronSchedule [Horizon]
  deriving (Show, Eq, Typeable)

data ForecastTimeIx = ForecastTimeIx Time Horizon
  deriving (Eq, Ord, Show, Typeable)

instance IsDimension ForecastTimeDimension where
    type DimIx ForecastTimeDimension = ForecastTimeIx
