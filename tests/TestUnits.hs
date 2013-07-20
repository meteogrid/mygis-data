{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TestUnits (tests) where

import Control.Applicative (liftA)
import Data.Vector.Unboxed as V
import Prelude as P
import Test.Framework
import Test.Framework.TH
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

import MyGIS.Data.Units as U


tests :: Test.Framework.Test
tests = $(testGroupGenerator)

instance Arbitrary (Length Double) where
    arbitrary = liftA (*~ meter) arbitrary

prop_can_operate_on_quantity_unboxed_vectors ::
  Length Double -> [Length Double] -> Bool
prop_can_operate_on_quantity_unboxed_vectors v vs =
    (V.toList . V.map fn . V.fromList)  vs  ==
    P.map fn vs
  where
    fn :: Length Double -> Area Double
    fn = (U.*) v
