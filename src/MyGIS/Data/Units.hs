{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MyGIS.Data.Units (
    IsUnit

  , module Numeric.Units.Dimensional.TF
  , module Numeric.Units.Dimensional.TF.Quantities
  , module Numeric.Units.Dimensional.TF.SIUnits
  , module Numeric.Units.Dimensional.TF.NonSI
) where

import           Prelude (Show(..), Eq(..))
import           Data.Data (Data)
import           Data.Vector.Generic.Base (Vector)
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Unboxed as U
import           Numeric.Units.Dimensional.TF
import           Numeric.Units.Dimensional.TF.Quantities
import           Numeric.Units.Dimensional.TF.SIUnits
import           Numeric.Units.Dimensional.TF.NonSI

class (Data u, Show u, Eq u) => IsUnit u

-- TODO: Template Haskell for the followinig bolierplate?
instance Show (Unit DLength a) where show _ = "length"
instance (Data a, Show a, Eq a) => IsUnit (Unit DLength a) where
deriving instance Vector U.Vector a   => (Vector U.Vector) (Length a)
deriving instance MVector U.MVector a => (MVector U.MVector) (Length a)
deriving instance U.Unbox a           => U.Unbox (Length a)

instance Show (Unit DArea a) where show _ = "area"
instance (Data a, Show a, Eq a) => IsUnit (Unit DArea a) where
deriving instance Vector U.Vector a   => (Vector U.Vector) (Area a)
deriving instance MVector U.MVector a => (MVector U.MVector) (Area a)
deriving instance U.Unbox a           => U.Unbox (Area a)

instance Show (Unit DVolume a) where show _ = "volume"
instance (Data a, Show a, Eq a) => IsUnit (Unit DVolume a) where
deriving instance Vector U.Vector a   => (Vector U.Vector) (Volume a)
deriving instance MVector U.MVector a => (MVector U.MVector) (Volume a)
deriving instance U.Unbox a           => U.Unbox (Volume a)

instance Show (Unit DMass a) where show _ = "mass"
instance (Data a, Show a, Eq a) => IsUnit (Unit DMass a) where
deriving instance Vector U.Vector a   => (Vector U.Vector) (Mass a)
deriving instance MVector U.MVector a => (MVector U.MVector) (Mass a)
deriving instance U.Unbox a           => U.Unbox (Mass a)

instance Show (Unit DThermodynamicTemperature a) where show _ = "temperature"
instance (Data a, Show a, Eq a) => IsUnit (Unit DThermodynamicTemperature a)
  where
deriving instance Vector U.Vector a   =>
    (Vector U.Vector) (ThermodynamicTemperature a)
deriving instance MVector U.MVector a =>
    (MVector U.MVector) (ThermodynamicTemperature a)
deriving instance U.Unbox a           => U.Unbox (ThermodynamicTemperature a)

instance Show (Unit DTime a) where show _ = "time"
instance (Data a, Show a, Eq a) => IsUnit (Unit DTime a) where
deriving instance Vector U.Vector a   => (Vector U.Vector) (Time a)
deriving instance MVector U.MVector a => (MVector U.MVector) (Time a)
deriving instance U.Unbox a           => U.Unbox (Time a)

instance Show (Unit DVelocity a) where show _ = "velocity"
instance (Data a, Show a, Eq a) => IsUnit (Unit DVelocity a) where
deriving instance Vector U.Vector a   => (Vector U.Vector) (Velocity a)
deriving instance MVector U.MVector a => (MVector U.MVector) (Velocity a)
deriving instance U.Unbox a           => U.Unbox (Velocity a)

instance Show (Unit DOne a) where show _ = "dimensionless"
instance (Data a, Show a, Eq a) => IsUnit (Unit DOne a) where
deriving instance Vector U.Vector a   => (Vector U.Vector) (Dimensionless a)
deriving instance MVector U.MVector a => (MVector U.MVector) (Dimensionless a)
deriving instance U.Unbox a           => U.Unbox (Dimensionless a)
