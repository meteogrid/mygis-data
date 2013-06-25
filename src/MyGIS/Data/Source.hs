{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module MyGIS.Data.Source (
    Source (Source)
  , IsSource (..)
  , RasterSource (RasterSource)

  , store
  , dimIx
) where


import           MyGIS.Data.Dimension (DimensionIx)
import           MyGIS.Data.Store (IsStore, RasterStore)

data Source st = Source {
    store :: st
  , dimIx :: DimensionIx
} deriving (Eq, Show)

class IsStore st => IsSource s st | st -> s  where
    toSource :: s -> Source st


data RasterSource st = RasterSource {
    rsStore   :: st
  , rsDimIx   :: DimensionIx
} deriving (Eq, Show)


instance IsSource (RasterSource RasterStore) RasterStore where
    toSource rs = Source (rsStore rs) (rsDimIx rs)
