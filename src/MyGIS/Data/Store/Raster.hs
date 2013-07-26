{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module MyGIS.Data.Store.Raster (
  RasterStore (..)
) where

import           Data.Typeable (Typeable)
--import           Data.Vector.Unboxed as V
import           MyGIS.Data.Dimension
import           MyGIS.Data.Units (Unit)
import           MyGIS.Data.Store.Types


data RasterStore d u t = RasterStore {
    rsDim     :: d
  , rsUnits   :: Unit u t
  , rsId      :: StoreID
} deriving (Eq, Show, Typeable)

instance (IsDimension d, Typeable t, Show t, Typeable u, Show u, Num t, Eq t)
  => IsStore RasterStore d u t where
    type Src RasterStore d u t = RasterSource (RasterStore d u t) (DimIx d)
    getSource                  = RasterSource
    dimension                  = rsDim
    sId                        = rsId
    units                      = rsUnits


data RasterSource s ix = RasterSource s Context ix
  deriving (Eq, Show)

{-
data RasterView u = RasterView {
    vContext :: Context
  , vData    :: V.Vector u
} deriving (Eq, Show)


getView :: RasterSource s ix -> Context -> m (RasterView)
getView = undefined
-}
