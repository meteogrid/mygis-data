{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module MyGIS.Data.Store.Raster (
  RasterStore (..)
) where

import           Data.Typeable (Typeable2)
--import           Data.Vector.Unboxed as V
import           MyGIS.Data.Context
import           MyGIS.Data.Dimension
import           MyGIS.Data.Units (IsUnit)
import           MyGIS.Data.Store.Generic


data RasterStore d u where {
    RasterStore :: (IsUnit u, IsDimension d) => {
        rsId      :: StoreId
      , rsContext :: Context
      , rsDim     :: d
      , rsUnits   :: u
      } -> RasterStore d u
}
deriving instance Eq (RasterStore d u)
deriving instance Show (RasterStore d u)
deriving instance Typeable2 RasterStore

instance (IsUnit u, IsDimension d) => IsStore RasterStore d u where
    type Src RasterStore d u = RasterSource (RasterStore d u) (DimIx d)
    getSource                = RasterSource
    dimension                = rsDim
    storeId                  = rsId
    context                  = rsContext
    units                    = rsUnits


data RasterSource s ix = RasterSource s ix
  deriving (Eq, Show)

{-
data RasterView u = RasterView {
    vContext :: Context
  , vData    :: V.Vector u
} deriving (Eq, Show)


getView :: RasterSource s ix -> Context -> m (RasterView)
getView = undefined
-}
