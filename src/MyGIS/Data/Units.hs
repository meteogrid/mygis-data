{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MyGIS.Data.Units (
    RealUnit
  , IntUnit
  , RealLength
  , meter
) where

import           Numeric.Units.Dimensional.TF
import           Numeric.Units.Dimensional.TF.SIUnits

type RealUnit a = Unit a Double
type IntUnit a = Unit a Int

type RealLength = RealUnit DLength
instance Show RealLength where show _ = "length (meter)"
