{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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
import           MyGIS.Data.Dimension (Dimension)



data Store d = Store {
    type_     :: Text
 ,  context   :: Context
 ,  dimension :: d
} deriving (Eq, Show)

class Dimension d ix => IsStore st d ix where
    toStore :: st -> Store d

instance Dimension d ix => IsStore (Store d) d ix where
    toStore = id 

data RasterStore d = RasterStore {
    rsType    :: Text
  , rsContext :: Context
  , rsDim     :: d
} deriving (Eq, Show)


instance Dimension d ix => IsStore (RasterStore d) d ix where
    toStore rs = Store {
        type_      = rsType rs
      , context    = rsContext rs
      , dimension  = rsDim rs
      }
