{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module TestContext (tests) where

import Data.Maybe (isJust, isNothing)
import Test.QuickCheck
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
-- import Test.Framework.Providers.HUnit
-- import Test.HUnit.Base hiding (Test)

import MyGIS.Data.Context


instance Arbitrary Envelope where
    arbitrary = do
        x0 <- choose (-5e5,5e6)
        y0 <- choose (-5e5,5e6)
        w  <- choose (1,5e6) :: Gen Double
        h  <- choose (1,5e6) :: Gen Double
        let Right r = mkEnvelope x0 y0 (x0 + w) (y0 + h)
        return r

instance Arbitrary Shape where
    arbitrary = do
        w <- choose (1,9e3) :: Gen Double
        h <- choose (1,9e3) :: Gen Double
        let Right r = mkShape (round w) (round h)
        return r

instance Arbitrary Context where
    arbitrary = do
        e <- arbitrary
        s <- arbitrary
        return $ Context "" e s ""

instance Arbitrary Pixel where
    arbitrary = do
        x <- choose (-9e3, 9e3) :: Gen Double
        y <- choose (-9e3, 9e3) :: Gen Double
        return $ Pixel (round x, round y)

instance Arbitrary Point where
    arbitrary = do
        x <- choose (-9e3,9e3) :: Gen Double
        y <- choose (-9e3,9e3) :: Gen Double
        return $ Point (x,y)

prop_envelopes_equality :: Envelope -> Envelope -> Bool
prop_envelopes_equality a b
    | a==b      = all (\f -> f a == f b) [minx, miny, maxx, maxy]
    | otherwise = all (\f -> f a /= f b) [minx, miny, maxx, maxy]


eitherFails, eitherSucceeds :: forall a b. Either a b -> Bool
eitherFails = either (\_ -> True) (\_ -> False)
eitherSucceeds = either (\_ -> False) (\_ -> True)

isValidConstruction :: forall a b. Bool -> Either a b -> Bool
isValidConstruction cond ctr = if cond then eitherSucceeds ctr
                                       else eitherFails ctr

prop_mkEnvelope_only_constructs_correct_envelopes ::
    Double -> Double -> Double -> Double -> Bool
prop_mkEnvelope_only_constructs_correct_envelopes x0 y0 x1 y1
    = isValidConstruction (x0<x1 && y0<y1) $ mkEnvelope x0 y0 x1 y1

prop_mkShape_only_constructs_correct_shapes :: Int -> Int -> Bool
prop_mkShape_only_constructs_correct_shapes w h
    = isValidConstruction (w>0 && h>0) $ mkShape w h

prop_all_shapes_intersect :: Shape -> Shape -> Bool
prop_all_shapes_intersect = intersects

almostEqual :: forall a. (Num a, Ord a) => a -> a -> a -> Bool
almostEqual a b e = abs (a - b) < e

prop_forward_backward_is_id :: Context -> Pixel -> Bool
prop_forward_backward_is_id ctx px
    = let bf = (forward ctx) . (backward ctx)
      in bf px == px

prop_backward_forward_is_almost_id :: Context -> Point -> Bool
prop_backward_forward_is_almost_id ctx pt
    = let fb            = (backward ctx) . (forward ctx)
          e             = (envelope ctx)
          Point (x,y)   = pt
          Point (x',y') = fb pt
          rtol          = 0.01
          epsilon       = max (width e * rtol) (height e * rtol)
      in (abs (x-x') < epsilon) && (abs (y-y') < epsilon)


prop_intersects_behaves_as_model :: Envelope -> Envelope -> Bool
prop_intersects_behaves_as_model a b
    = intersects a b == intersects' a b


prop_intersection_conmutes :: Envelope -> Envelope -> Bool
prop_intersection_conmutes a b = intersection a b == intersection a b

prop_intersection_of_same_env_is_id :: Envelope -> Bool
prop_intersection_of_same_env_is_id a = intersection a a == Just a

prop_intersection_is_smaller_or_eq_to_envs :: Envelope -> Envelope -> Property
prop_intersection_is_smaller_or_eq_to_envs a b =
    intersects' a b ==> let (Just i)    = intersection a b
                       in     width i  <= width a
                           && width i  <= width b
                           && height i <= height b
                           && height i <= height a

prop_intersection_is_Nothing_if_no_intersect :: Envelope -> Envelope -> Property
prop_intersection_is_Nothing_if_no_intersect a b =
    not (intersects' a b) ==> isNothing (intersection a b)

prop_intersection_is_Just_if_intersect :: Envelope -> Envelope -> Property
prop_intersection_is_Just_if_intersect a b =
    intersects' a b ==> isJust (intersection a b)

intersects' :: Envelope -> Envelope -> Bool
intersects' a b = not ( (minx a >= maxx b) ||
                        (maxx a <= minx b) ||
                        (miny a >= maxy b) ||
                        (maxy a <= miny b) )

tests = $(testGroupGenerator)
