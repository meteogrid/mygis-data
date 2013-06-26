{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module MyGIS.Data.Store (
    IsStore (..)
  , Store (Store)
  , RasterStore (RasterStore)

  , type_
  , context
  , dimension
) where

import           Data.Text (Text)
import           MyGIS.Data.Context (Context)
import           MyGIS.Data.Dimension (Dimension, DimIx)
import           MyGIS.Data.Source (RasterSource)



data Store d = Store {
    type_     :: Text
 ,  context   :: Context
 ,  dimension :: d
} deriving (Eq, Show)

class Dimension d => IsStore st d where
    type Src st d :: *
    toStore :: st d -> Store d
    getSource :: st d -> DimIx d -> Src st d


data RasterStore d = RasterStore {
    rsType    :: Text
  , rsContext :: Context
  , rsDim     :: d
} deriving (Eq, Show)


instance Dimension d => IsStore RasterStore d where
    type Src RasterStore d = RasterSource (DimIx d)
    toStore rs = Store {
        type_      = rsType rs
      , context    = rsContext rs
      , dimension  = rsDim rs
      }
    getSource = undefined
