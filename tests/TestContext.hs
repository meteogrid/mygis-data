{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module TestContext (tests) where

import Control.Applicative ((<*>), (<$>), pure, liftA)
import Data.Monoid (Monoid(mempty))
import Test.QuickCheck
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import MyGIS.Data.Context


tests :: Test.Framework.Test
tests = $(testGroupGenerator)

instance Arbitrary Envelope where
    arbitrary = do
        x0 <- choose (-5e5,5e6)
        y0 <- choose (-5e5,5e6)
        w  <- choose (0,5e6) :: Gen Double
        h  <- choose (0,5e6) :: Gen Double
        return $ mkEnvelope x0 y0 (x0 + w) (y0 + h)

instance Arbitrary Shape where
    arbitrary = mkShape <$> liftA round num <*> liftA round num
      where num = choose (0, 6e3) :: Gen Double

instance Arbitrary Context where
    arbitrary = do
        ctx <- mkContext <$> pure "" <*> arbitrary <*> arbitrary <*> pure ""
        either (\_ -> arbitrary) return ctx

instance Arbitrary Pixel where
    arbitrary = Pixel <$> liftA round num <*> liftA round num
      where num = choose (0, 6e3) :: Gen Double

instance Arbitrary Point where
    arbitrary = Point <$> choose (-9e3,9e3) <*> choose (-9e3,9e3)

prop_envelopes_equality :: Envelope -> Envelope -> Bool
prop_envelopes_equality a b
    | a==b      = all (\f -> f a == f b) [minx, miny, maxx, maxy]
    | otherwise = all (\f -> f a /= f b) [minx, miny, maxx, maxy]


isValidConstruction :: (Ord a, Num a) => Bool -> Box a -> Bool
isValidConstruction cond ctr = if cond then ctr /= mempty
                                       else ctr == mempty

prop_mkEnvelope_only_constructs_correct_envelopes ::
    Double -> Double -> Double -> Double -> Bool
prop_mkEnvelope_only_constructs_correct_envelopes x0 y0 x1 y1
    = isValidConstruction (x0<x1 && y0<y1) $ mkEnvelope x0 y0 x1 y1

prop_mkShape_only_constructs_correct_shapes :: Int -> Int -> Bool
prop_mkShape_only_constructs_correct_shapes w h
    = isValidConstruction (w>0 && h>0) $ mkShape w h

prop_all_shapes_intersect :: Shape -> Shape -> Bool
prop_all_shapes_intersect = intersects

prop_forward_backward_is_id :: Context -> Pixel -> Bool
prop_forward_backward_is_id ctx px
    = px == ((forward ctx) . (backward ctx)) px


prop_backwardS_forwardS_is_almost_id :: Context -> Point -> Bool
prop_backwardS_forwardS_is_almost_id ctx pt
    = let f           = (backwardS ctx) . (forwardS ctx)
          e           = (envelope ctx)
          Point x  y  = pt
          Point x' y' = f pt
          epsilon     = 0.00001 * (max (width e ) (height e))
          a ~= b      = abs (a - b) < epsilon
      in x ~= x' && y ~= y'


prop_intersection_conmutes :: Envelope -> Envelope -> Bool
prop_intersection_conmutes a b = intersection a b == (flip intersection) a b

prop_intersection_of_same_env_is_id :: Envelope -> Bool
prop_intersection_of_same_env_is_id a = intersection a a == a

prop_intersection_with_empty_is_empty :: Envelope -> Bool
prop_intersection_with_empty_is_empty a = intersection mempty a == mempty

prop_intersection_is_smaller_or_eq_to_envs :: Envelope -> Envelope -> Property
prop_intersection_is_smaller_or_eq_to_envs a b =
    intersects a b ==> let i = intersection a b
                       in    width  i <= width a
                          && width  i <= width b
                          && height i <= height b
                          && height i <= height a

prop_intersection_is_empty_if_intersects :: Envelope -> Envelope -> Property
prop_intersection_is_empty_if_intersects a b =
    not (intersects a b) ==> isEmpty (intersection a b)

prop_intersection_is_not_empty_if_intersects :: Envelope -> Envelope -> Property
prop_intersection_is_not_empty_if_intersects a b =
    intersects a b ==> not . isEmpty $ intersection a b

prop_union_conmutes :: Envelope -> Envelope -> Bool
prop_union_conmutes a b = union a b == (flip union) a b

prop_union_of_same_env_is_id :: Envelope -> Bool
prop_union_of_same_env_is_id a = union a a == a

prop_union_with_empty_is_id :: Envelope -> Bool
prop_union_with_empty_is_id a = union mempty a == a
