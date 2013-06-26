{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module MyGIS.Data.Store (
    IsStore (..)
  , AnyStore(..)
  , RealLength
  , RasterStore (RasterStore)
  , sContext
  , sType
) where

import           Data.Text (Text)
import           Data.Typeable (Typeable, cast)
import           Numeric.Units.Dimensional.TF (Unit, DLength)
import           MyGIS.Data.Context (Context)
import           MyGIS.Data.Dimension (Dimension, DimIx)
import           MyGIS.Data.Source (RasterSource(RasterSource))

type Type = Text

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
    dim             :: st d u -> d
    units           :: st d u -> u
    getSource       :: st d u -> DimIx d -> Src st d u

    fromAnyStore (AnyStore s _ _) = cast s

data RasterStore d u = RasterStore {
    rsType    :: Text
  , rsContext :: Context
  , rsDim     :: d
  , rsUnits   :: u
} deriving (Eq, Show, Typeable)

class (Typeable u, Show u, Eq u) => IsUnit u where

type RealLength = Unit DLength Double
instance IsUnit RealLength where

instance Show RealLength where show _ = "RealLength"

instance (IsUnit u, Dimension d) => IsStore RasterStore d u where
    type Src RasterStore d u = RasterSource (DimIx d) u
    toAnyStore rs            = AnyStore rs (rsType rs) (rsContext rs)
    getSource rs ix          = RasterSource ix (rsUnits rs)
    dim                      = rsDim
    units                    = rsUnits
