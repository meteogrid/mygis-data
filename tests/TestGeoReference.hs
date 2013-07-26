{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module TestGeoReference (tests) where

import Control.Applicative ((<*>), (<$>), pure, liftA)
import Data.Monoid (Monoid(mempty))
import Test.QuickCheck
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import SIGyM.GeoReference


tests :: Test.Framework.Test
tests = $(testGroupGenerator)

instance Arbitrary Extent where
    arbitrary = do
        x0 <- choose (-5e5,5e6)
        y0 <- choose (-5e5,5e6)
        w  <- choose (0,5e6) :: Gen Double
        h  <- choose (0,5e6) :: Gen Double
        return $ mkExtent x0 y0 (x0 + w) (y0 + h)

instance Arbitrary Shape where
    arbitrary = mkShape <$> liftA round num <*> liftA round num
      where num = choose (0, 6e3) :: Gen Double

instance Arbitrary GeoReference where
    arbitrary = do
        ctx <- mkGeoReference <$> arbitrary <*> arbitrary <*> pure ""
        either (\_ -> arbitrary) return ctx

instance Arbitrary Pixel where
    arbitrary = Pixel <$> liftA round num <*> liftA round num
      where num = choose (0, 6e3) :: Gen Double

instance Arbitrary Point where
    arbitrary = Point <$> choose (-9e3,9e3) <*> choose (-9e3,9e3)

prop_extents_equality :: Extent -> Extent -> Bool
prop_extents_equality a b
    | a==b      = all (\f -> f a == f b) [minx, miny, maxx, maxy]
    | otherwise = all (\f -> f a /= f b) [minx, miny, maxx, maxy]


isValidConstruction :: (Ord a, Num a) => Bool -> Box a -> Bool
isValidConstruction cond ctr = if cond then ctr /= mempty
                                       else ctr == mempty

prop_mkExtent_only_constructs_correct_extents ::
    Double -> Double -> Double -> Double -> Bool
prop_mkExtent_only_constructs_correct_extents x0 y0 x1 y1
    = isValidConstruction (x0<x1 && y0<y1) $ mkExtent x0 y0 x1 y1

prop_mkShape_only_constructs_correct_shapes :: Int -> Int -> Bool
prop_mkShape_only_constructs_correct_shapes w h
    = isValidConstruction (w>0 && h>0) $ mkShape w h

prop_all_shapes_intersect :: Shape -> Shape -> Bool
prop_all_shapes_intersect = intersects

prop_forward_backward_is_id :: GeoReference -> Pixel -> Bool
prop_forward_backward_is_id ctx px
    = px == ((forward ctx) . (backward ctx)) px


prop_backwardS_forwardS_is_almost_id :: GeoReference -> Point -> Bool
prop_backwardS_forwardS_is_almost_id ctx pt
    = let f           = (backwardS ctx) . (forwardS ctx)
          e           = (extent ctx)
          Point x  y  = pt
          Point x' y' = f pt
          epsilon     = 0.00001 * (max (width e ) (height e))
          a ~= b      = abs (a - b) < epsilon
      in x ~= x' && y ~= y'


prop_intersection_conmutes :: Extent -> Extent -> Bool
prop_intersection_conmutes a b = intersection a b == (flip intersection) a b

prop_intersection_associates :: Extent -> Extent -> Extent -> Bool
prop_intersection_associates a b c = intersection a (intersection b c) == intersection (intersection a b) c

prop_intersection_of_same_extent_is_id :: Extent -> Bool
prop_intersection_of_same_extent_is_id a = intersection a a == a

prop_intersection_with_empty_is_empty :: Extent -> Bool
prop_intersection_with_empty_is_empty a = intersection mempty a == mempty

prop_intersection_is_smaller_or_eq_to_extents :: Extent -> Extent -> Property
prop_intersection_is_smaller_or_eq_to_extents a b =
    intersects a b ==> let i = intersection a b
                       in    width  i <= width a
                          && width  i <= width b
                          && height i <= height b
                          && height i <= height a

prop_intersection_is_empty_if_intersects :: Extent -> Extent -> Property
prop_intersection_is_empty_if_intersects a b =
    not (intersects a b) ==> isEmpty (intersection a b)

prop_intersection_is_not_empty_if_intersects :: Extent -> Extent -> Property
prop_intersection_is_not_empty_if_intersects a b =
    intersects a b ==> not . isEmpty $ intersection a b

prop_union_conmutes :: Extent -> Extent -> Bool
prop_union_conmutes a b = union a b == (flip union) a b

prop_union_associates :: Extent -> Extent -> Extent -> Bool
prop_union_associates a b c = union a (union b c) == union (union a b) c


prop_union_of_same_extent_is_id :: Extent -> Bool
prop_union_of_same_extent_is_id a = union a a == a

prop_union_with_empty_is_id :: Extent -> Bool
prop_union_with_empty_is_id a = union mempty a == a

prop_union_is_larger_or_eq_to_extents :: Extent -> Extent -> Bool
prop_union_is_larger_or_eq_to_extents a b =
    let i = union a b
    in    width  i >= width a
       && width  i >= width b
       && height i >= height b
       && height i >= height a
