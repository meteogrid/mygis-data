{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module MyGIS.Data.Store.Raster (
  RasterStore (..)
) where

import           Data.Typeable
import           Data.Data
import           Data.Vector.Unboxed as V
import           Data.Text (Text)
import           MyGIS.Data.Context
import           MyGIS.Data.Dimension
import           MyGIS.Data.Units (IsUnit)
import           MyGIS.Data.Store.Generic


data RasterStore d u = RasterStore {
    rsUnits   :: u
  , rsDim     :: d
  , rsName    :: Text
  , rsContext :: Context
} deriving (Eq, Show, Typeable, Data)

instance (IsUnit u, IsDimension d) => IsStore RasterStore d u where
    type Src RasterStore d u = RasterSource (RasterStore d u) (DimIx d)
    getSource rs ix          = RasterSource rs ix
    dimension                = rsDim
    name                     = rsName
    context                  = rsContext


data RasterSource s ix = RasterSource s ix
  deriving (Eq, Show)

data RasterView u = RasterView {
    vContext :: Context
  , vUnits   :: u
  , vData    :: V.Vector Double
} deriving (Eq, Show)


getView :: RasterSource s ix -> Context -> m (RasterView)
getView = undefined
