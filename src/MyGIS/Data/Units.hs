{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module MyGIS.Data.Units (
    IsUnit

  , module Numeric.Units.Dimensional.TF
  , module Numeric.Units.Dimensional.TF.Quantities
  , module Numeric.Units.Dimensional.TF.SIUnits
  , module Numeric.Units.Dimensional.TF.NonSI
) where

import           Prelude (Show(..), Eq(..), Fractional, (++), (/), undefined)
import           Data.Typeable (Typeable)
import           Data.Vector.Generic.Base (Vector)
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Unboxed as U
import           Numeric.Units.Dimensional.TF
import           Numeric.Units.Dimensional.TF.Quantities
import           Numeric.Units.Dimensional.TF.SIUnits
import           Numeric.Units.Dimensional.TF.NonSI

class (Typeable u, Show u, Eq u) => IsUnit u where

instance (Typeable a, Show a, Eq a, Typeable d, Show (Unit d a)) =>
    IsUnit (Unit d a) where

deriving instance forall d a. Vector U.Vector a =>
    (Vector U.Vector) (Quantity d a)
deriving instance forall d a. MVector U.MVector a =>
    (MVector U.MVector) (Quantity d a)
deriving instance forall d a. U.Unbox a =>
    U.Unbox (Quantity d a)

instance forall d a. (Show d, Show a, Fractional a) => Show (Unit d a) where
    show (Dimensional x) = show (undefined :: d)
                           ++ " ("  ++ show x ++ ")"
