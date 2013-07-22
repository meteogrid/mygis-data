{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module MyGIS.Data.Store.Generic (
    IsStore (..)
  , Store (..)
  , StoreID (..)
  , downCast
) where

import           Data.Text (Text)
import           Data.Typeable (Typeable, cast)
import           MyGIS.Data.Context
import           MyGIS.Data.Dimension (IsDimension(..), DimIx)
import           MyGIS.Data.Units (Unit)


newtype StoreID = ID Text deriving (Eq,Ord,Show,Typeable)

class ( IsDimension d
      , Eq (st d u t)
      , Show (st d u t)
      , Typeable (st d u t)
      ) =>  IsStore st d u t
  where
    type Src st d u t :: *

    storeId    :: st d u t -> StoreID
    getSource  :: st d u t -> Context -> DimIx d -> Src st d u t
    getSources :: st d u t -> Context -> DimIx d -> DimIx d -> [Src st d u t]
    dimension  :: st d u t -> d
    units      :: st d u t -> Unit u t

    getSources s ctx from to =
        map (getSource s ctx) (enumFromToIx (dimension s) from to)

downCast :: IsStore st d u t => Store -> Maybe (st d u t)
downCast (Store s) = cast s


data Store where
  Store :: IsStore st d u t => st d u t -> Store
