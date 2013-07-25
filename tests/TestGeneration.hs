{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGeneration (tests) where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import MyGIS.Data.Store


tests :: Test.Framework.Test
tests = $(testGroupGenerator)

case_can_return_good_values :: IO ()
case_can_return_good_values = do
  r <- evalGen' (return 1) :: IO (Either GenError Int)
  assertEqual "not a right value" (Right 1) r

case_can_throw_error :: IO ()
case_can_throw_error = do
  r <- evalGen' (throwError noMsg) :: IO (Either GenError Int)
  assertEqual "not a left value" (Left noMsg) r

case_can_getTime :: IO ()
case_can_getTime = do
  Right t <- evalGen' (return getTime)
  return ()

evalGen' :: Generation a -> IO (Either GenError a)
evalGen' a = do
  env <- mkEnvironment
  evalGen env GenState a
