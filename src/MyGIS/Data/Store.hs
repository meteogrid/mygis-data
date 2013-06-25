
module MyGIS.Data.Store (
    IsStore (..)
  , Store (Store)
  , RasterStore (RasterStore)

  , type_
  , context
) where

import           Data.Text (Text)
import           MyGIS.Data.Context (Context)



data Store = Store {
    type_   :: Text
 ,  context :: Context
} deriving (Eq, Show)

class IsStore st where
    toStore :: st -> Store

instance IsStore Store where
    toStore = id 

data RasterStore = RasterStore {
    rsType    :: Text
  , rsContext :: Context
} deriving (Eq, Show)


instance IsStore RasterStore where
    toStore rs = Store {
        type_   = rsType rs
      , context = rsContext rs
      }
