{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}

module TestContext where

import Test.QuickCheck
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
-- import Test.Framework.Providers.HUnit
-- import Test.HUnit.Base hiding (Test)

import MyGIS.Data.Context


instance Arbitrary Envelope where
    arbitrary = do
        x0 <- choose (-1000,1000)
        y0 <- choose (-1000,1000)
        w  <- choose (1,1000)
        h  <- choose (1,1000)
        let Right r = mkEnvelope x0 y0 (x0 + w) (y0 + h)
        return r

prop_envelopes_equality :: Envelope -> Envelope -> Bool
prop_envelopes_equality a b
    | a==b      = all (\f -> f a == f b) [minx, miny, maxx, maxy]
    | otherwise = all (\f -> f a /= f b) [minx, miny, maxx, maxy]


prop_mkEnvelope_only_constructs_correct_envelopes ::
    Double -> Double -> Double -> Double -> Bool
prop_mkEnvelope_only_constructs_correct_envelopes x0 y0 x1 y1
    | x0<x1 && y0<y1
        = case mkEnvelope x0 y0 x1 y1 of
               Right _ -> True
               Left _  -> False
    | otherwise
        = case mkEnvelope x0 y0 x1 y1 of
               Right _ -> False
               Left _  -> True


tests = $(testGroupGenerator)
