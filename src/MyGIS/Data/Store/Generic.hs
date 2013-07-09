{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module MyGIS.Data.Store.Generic (
    IsStore (..)
  , StoreId (..)
) where

import           Data.Data (Data, Typeable)
import           MyGIS.Data.Context
import           MyGIS.Data.Dimension (IsDimension(..), DimIx)
import           MyGIS.Data.Units (IsUnit)

newtype StoreId = ID (String,String) deriving (Eq,Ord,Show,Typeable,Data)

class (IsUnit u, IsDimension d, Eq (st d u), Show (st d u), Data (st d u))
  =>  IsStore st d u
  where
    type Src st d u :: *

    storeId     :: st d u -> StoreId
    context     :: st d u -> Context
    getSource   :: st d u -> DimIx d -> Src st d u
    getSources  :: st d u -> DimIx d -> DimIx d -> [Src st d u]
    dimension   :: st d u -> d
    units       :: st d u -> u

    getSources s from to =
        map (getSource s) (enumFromToIx (dimension s) from to)

