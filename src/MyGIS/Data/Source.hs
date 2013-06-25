{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module MyGIS.Data.Source (
    Source (Source)
  , IsSource (..)
  , RasterSource (RasterSource)
) where


import           MyGIS.Data.Dimension (DimensionIx)
import           MyGIS.Data.Store (IsStore)

data Source st s where
    Source :: IsSource st s => s -> Source st s


class IsSource st s | st -> s where
    store :: IsStore st => s -> st
    dimIx :: s -> DimensionIx


data RasterSource st = RasterSource {
    rsStore   :: st
  , rsDimIx   :: DimensionIx
} deriving (Eq, Show)


instance IsSource st (RasterSource st) where
    store    = rsStore
    dimIx   = rsDimIx
