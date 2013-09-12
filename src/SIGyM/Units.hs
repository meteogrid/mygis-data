{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module SIGyM.Units (
  IsUnit (..)
  , module Numeric.Units.Dimensional.TF
  , module Numeric.Units.Dimensional.TF.Quantities
  , module Numeric.Units.Dimensional.TF.SIUnits
  , module Numeric.Units.Dimensional.TF.NonSI
) where

import           Prelude ( Show(..), Eq(..), Num, Double, (++), undefined)
import           Data.Vector.Generic.Base (Vector)
import           Data.Vector.Generic.Mutable (MVector)
import           Data.Vector.Storable (Storable)
import qualified Data.Vector.Unboxed as U
import           Numeric.Units.Dimensional.TF
import           Numeric.Units.Dimensional.TF.Quantities
import           Numeric.Units.Dimensional.TF.SIUnits
import           Numeric.Units.Dimensional.TF.NonSI

class IsUnit u where
    type AssocQuantity u :: *
    (.*.) :: Double -> u -> AssocQuantity u
    (./.) :: AssocQuantity u -> u -> Double
infixl 7 .*.
infixl 7 ./.

instance forall a. IsUnit (Unit a Double) where
    type AssocQuantity (Unit a Double) = Quantity a Double
    (.*.) = (*~)
    (./.) = (/~)


deriving instance Vector U.Vector a => (Vector U.Vector) (Quantity d a)
deriving instance MVector U.MVector a => (MVector U.MVector) (Quantity d a)
deriving instance U.Unbox a => U.Unbox (Quantity d a)
deriving instance Storable a => Storable (Quantity d a)

instance (Show d, Show a, Num a) => Show (Unit d a) where
    show (Dimensional x) = show (undefined :: d) ++ " ("  ++ show x ++ ")"


