{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module SIGyM.Units (
    module Numeric.Units.Dimensional.TF
  , module Numeric.Units.Dimensional.TF.Quantities
  , module Numeric.Units.Dimensional.TF.SIUnits
  , module Numeric.Units.Dimensional.TF.NonSI
) where

import           Prelude ( Show(..), Eq(..), Num, (++), undefined)
import           Data.Vector.Generic.Base (Vector)
import           Data.Vector.Generic.Mutable (MVector)
import           Data.Vector.Storable (Storable)
import qualified Data.Vector.Unboxed as U
import           Numeric.Units.Dimensional.TF
import           Numeric.Units.Dimensional.TF.Quantities
import           Numeric.Units.Dimensional.TF.SIUnits
import           Numeric.Units.Dimensional.TF.NonSI


deriving instance Vector U.Vector a => (Vector U.Vector) (Quantity d a)
deriving instance MVector U.MVector a => (MVector U.MVector) (Quantity d a)
deriving instance U.Unbox a => U.Unbox (Quantity d a)
deriving instance Storable a => Storable (Quantity d a)

instance (Show d, Show a, Num a) => Show (Unit d a) where
    show (Dimensional x) = show (undefined :: d) ++ " ("  ++ show x ++ ")"

