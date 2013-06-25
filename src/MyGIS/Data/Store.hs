{-# LANGUAGE GADTs #-}

module MyGIS.Data.Store (
    IsStore (..)
  , Store (Store)
  , RasterStore (RasterStore)
) where

import           Data.Text (Text)
import           MyGIS.Data.Context (Context)



data Store where
    Store :: IsStore st => st -> Store


class IsStore st where
    type_   :: st -> Text
    context :: st -> Context

data RasterStore = RasterStore {
    rsType    :: Text
  , rsContext :: Context
} deriving (Eq, Show)


instance IsStore RasterStore where
    type_   = rsType
    context = rsContext
