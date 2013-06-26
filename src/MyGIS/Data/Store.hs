{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module MyGIS.Data.Store (
    IsStore (..)
  , Store (Store)
  , RasterStore (RasterStore)

) where

import           Data.Text (Text)
import           Data.Typeable (Typeable, cast)
import           MyGIS.Data.Context (Context)
import           MyGIS.Data.Dimension (Dimension, DimIx)
import           MyGIS.Data.Source (RasterSource(RasterSource))



data Store where
    Store :: IsStore s d => s d -> Store

deriving instance Show Store
deriving instance Typeable Store

class (Eq (st d), Show (st d), Dimension d, Typeable (st d))  =>
  IsStore st d where
    type Src st d :: *
    fromStore :: Store -> Maybe (st d)
    fromStore (Store s) = cast s
    type_     :: st d -> Text
    context   :: st d -> Context
    dim       :: st d -> d
    getSource :: st d -> DimIx d -> Src st d


data RasterStore d = RasterStore {
    rsType    :: Text
  , rsContext :: Context
  , rsDim     :: d
} deriving (Eq, Show, Typeable)


instance Dimension d => IsStore RasterStore d where
    type Src RasterStore d = RasterSource (DimIx d)
    getSource st ix     = RasterSource ix
    type_               = rsType
    context             = rsContext
    dim                 = rsDim
