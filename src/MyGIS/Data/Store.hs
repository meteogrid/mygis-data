{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module MyGIS.Data.Store (
    IsStore (..)
  , AnyStore(..)
  , RasterStore (RasterStore)
  , sContext
  , sType
) where

import           Data.Typeable (Typeable, cast)
import           MyGIS.Data.Context (Context)
import           MyGIS.Data.Dimension (Dimension, DimIx, enumFromToIx)
import           MyGIS.Data.Source (RasterSource(RasterSource))
import           MyGIS.Data.Type (Type)


data AnyStore where
    AnyStore :: IsStore s d u => s d u -> Type -> Context -> AnyStore

sType :: AnyStore -> Type
sType (AnyStore _ t _) = t
sContext :: AnyStore -> Context
sContext (AnyStore _ _ c) = c

deriving instance Show AnyStore
deriving instance Typeable AnyStore

class (Typeable u, Eq (st d u), Show (st d u), Dimension d, Typeable (st d u))  =>
  IsStore st d u where
    type Src st d u :: *

    fromAnyStore    :: AnyStore -> Maybe (st d u)
    toAnyStore      :: st d u -> AnyStore
    getSource       :: st d u -> DimIx d -> Src st d u
    getSources      :: st d u -> DimIx d -> DimIx d -> [Src st d u]
    dimension       :: st d u -> d

    fromAnyStore (AnyStore s _ _) = cast s
    getSources s from to =
        map (getSource s) (enumFromToIx (dimension s) from to)

data RasterStore d u = RasterStore {
    rsType    :: Type
  , rsContext :: Context
  , rsDim     :: d
  , rsUnits   :: u
} deriving (Show, Typeable)

instance Eq (RasterStore d u) where
    s == s' =
      (rsType s) == (rsType s') && (rsContext s) == (rsContext s')


instance (Typeable u, Show u, Eq u, Dimension d)
  => IsStore RasterStore d u where
    type Src RasterStore d u = RasterSource (DimIx d) u
    toAnyStore rs            = AnyStore rs (rsType rs) (rsContext rs)
    getSource rs ix          = RasterSource ix (rsUnits rs)
    dimension                = rsDim
