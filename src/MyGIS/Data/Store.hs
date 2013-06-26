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

import           Data.Text (Text)
import           Data.Typeable (Typeable, cast)
import           MyGIS.Data.Context (Context)
import           MyGIS.Data.Dimension (Dimension, DimIx)
import           MyGIS.Data.Source (RasterSource(RasterSource))

type Type = Text

data AnyStore where
    AnyStore :: IsStore s d => s d -> Type -> Context -> AnyStore

sType :: AnyStore -> Type
sType (AnyStore _ t _) = t
sContext :: AnyStore -> Context
sContext (AnyStore _ _ c) = c

deriving instance Show AnyStore
deriving instance Typeable AnyStore

class (Eq (st d), Show (st d), Dimension d, Typeable (st d))  =>
  IsStore st d where
    type Src st d :: *
    fromAnyStore :: AnyStore -> Maybe (st d)
    toAnyStore   :: st d -> AnyStore
    fromAnyStore (AnyStore s _ _) = cast s
    dim       :: st d -> d
    getSource :: st d -> DimIx d -> Src st d


data RasterStore d = RasterStore {
    rsType    :: Text
  , rsContext :: Context
  , rsDim     :: d
} deriving (Eq, Show, Typeable)


instance Dimension d => IsStore RasterStore d where
    type Src RasterStore d = RasterSource (DimIx d)
    toAnyStore rs          = AnyStore rs (rsType rs) (rsContext rs)
    getSource st ix        = RasterSource ix
    dim                    = rsDim
