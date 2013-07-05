{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MyGIS.Data.Units (
    RealUnit
  , IsUnit
  , IntUnit
  , RealLength
  , meter
  , Dimensionless
) where

import           Data.Data (Data)
import           Numeric.Units.Dimensional.TF
import           Numeric.Units.Dimensional.TF.SIUnits

class (Data u, Show u, Eq u) => IsUnit u

type RealUnit a = Unit a Double
type IntUnit a = Unit a Int

type RealLength = RealUnit DLength
instance Show RealLength where show _ = "length (meter)"
instance IsUnit RealLength where
