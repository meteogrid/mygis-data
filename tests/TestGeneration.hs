{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGeneration (tests) where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Maybe (isJust)
import SIGyM.Units
import SIGyM.Store


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
  t <- evalGen' (return getTime)
  case t of
    Left _ -> assertFailure "Unexpected Left as result"
    _      -> return ()

dummyStore :: Store
dummyStore = RasterStore {
    rsId         = "dummy"
  , rsDimensions = []
  , rsUnits      = toDynUnits meter
  }

case_findStore_succeeds_on_registered_store :: IO ()
case_findStore_succeeds_on_registered_store = do
  let reg = registerStore emptyRegistry dummyStore
  assertBool "Unexpected error when registering store" (isJust reg)
  env <- mkEnvironment Nothing reg
  s <- evalGen env GenState (findStore (storeId dummyStore))
  assertEqual "could not findStore" (Right dummyStore) s
    
case_findStore_fails_on_unregistered_store :: IO ()
case_findStore_fails_on_unregistered_store = do
  s <- evalGen' $ findStore "foo"
  case s of
    Left (RegistryLookupError _) -> return ()
    Right _                      -> assertFailure "unexpected Right as result"
    Left _                       -> assertFailure "unexpected Left as result"


evalGen' :: Generation a -> IO (Either GenError a)
evalGen' a = do
  env <- mkEnvironment Nothing Nothing
  evalGen env GenState a
