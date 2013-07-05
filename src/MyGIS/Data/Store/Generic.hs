{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module MyGIS.Data.Store.Generic (
    IsStore (..)
) where

import           Data.Text (Text)
import           MyGIS.Data.Context
import           Data.Data (Data)
import           MyGIS.Data.Dimension (IsDimension(..), DimIx)
import           MyGIS.Data.Units (IsUnit)

class (IsUnit u, IsDimension d, Eq (st d u), Show (st d u), Data (st d u))
  =>  IsStore st d u
  where
    type Src st d u :: *

    name            :: st d u -> Text
    context         :: st d u -> Context
    getSource       :: st d u -> DimIx d -> Src st d u
    getSources      :: st d u -> DimIx d -> DimIx d -> [Src st d u]
    dimension       :: st d u -> d

    getSources s from to =
        map (getSource s) (enumFromToIx (dimension s) from to)

