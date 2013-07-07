{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TestUnits (tests) where

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
    arbitrary = do
        x <- arbitrary
        return (x *~ meter)

prop_can_operate_on_quantity_unboxed_vectors ::
  Length Double -> [Length Double] -> Bool
prop_can_operate_on_quantity_unboxed_vectors v vs =
    (V.toList . V.map fn . V.fromList)  vs  ==
    P.map fn vs
  where
    fn = (U.*) v
