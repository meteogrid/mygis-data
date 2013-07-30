{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module TestDynLoad (tests) where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.ByteString (ByteString, isInfixOf)
import Data.ByteString.Char8 (unpack)

import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath (joinPath)
import SIGyM.DynLoad


tests :: Test.Framework.Test
tests = $(testGroupGenerator)

case_can_load_double :: IO ()
case_can_load_double =
    withSystemTempDirectory "test." $ \tmpDir -> do
      withFile (joinPath [tmpDir, "Plugin.hs"]) WriteMode $ \f ->
        hPutStrLn f "module Plugin where sym = 2 :: Double"

      sym <- loadSymbolFromModule (defaultEnv { importPaths=[tmpDir] }) "Plugin" "sym"
      assertIsRight sym
      let Right num = sym
      assertEqual "loaded value is not correct" num (2 :: Double)

case_can_load_func :: IO ()
case_can_load_func =
    withSystemTempDirectory "test." $ \tmpDir -> do
      withFile (joinPath [tmpDir, "Plugin.hs"]) WriteMode $ \f ->
        hPutStrLn f
          "module Plugin where\nadd2 :: Double -> Double\nadd2 a = a+2"

      sym <- loadSymbolFromModule (defaultEnv { importPaths=[tmpDir] }) "Plugin" "add2"
      assertIsRight sym
      let Right fun = sym
          v = fun (4 :: Double)
      assertEqual "computed value is not correct" v (6 :: Double)

case_can_load_func_from_buffer :: IO ()
case_can_load_func_from_buffer = do
      sym <- loadSymbolFromBuffer defaultEnv "Plugin" "add2"
               "module Plugin where\nadd2 :: Double -> Double\nadd2 a = a+2"
      sym2 <- loadSymbolFromBuffer defaultEnv "Plugin" "add2"
               "module Plugin where\nadd2 :: Double -> Double\nadd2 a = a+3"
      assertIsRight sym
      assertIsRight sym2
      let Right fun  = sym
          Right fun2 = sym2
          v  = fun (4 :: Double)
          v2 = fun2 (4 :: Double)
      assertEqual "computed value v is not correct" v (6 :: Double)
      assertEqual "computed value v2 is not correct" v2 (7 :: Double)

case_import_sibling_modules :: IO ()
case_import_sibling_modules =
    withSystemTempDirectory "test." $ \tmpDir -> do
      withFile (joinPath [tmpDir, "Helpers.hs"]) WriteMode $ \f ->
        hPutStrLn f
          "module Helpers where\nadd2 :: Double -> Double\nadd2 a = a+2"
      let code = "module Plugin where\n\
                 \import Helpers\n\
                 \fun :: Double -> Double\n\
                 \fun = add2\n"
      withFile (joinPath [tmpDir, "Plugin.hs"]) WriteMode $ \f ->
        hPutStrLn f code

      sym <- loadSymbolFromModule (defaultEnv { importPaths=[tmpDir] }) "Plugin" "fun"
      assertIsRight sym
      let Right fun = sym
          v = fun (4 :: Double)
      assertEqual "computed value is not correct" v (6 :: Double)

case_compile_error_yields_Left_value :: IO ()
case_compile_error_yields_Left_value =
    withSystemTempDirectory "test." $ \tmpDir -> do
      withFile (joinPath [tmpDir, "Plugin.hs"]) WriteMode $ \f ->
        hPutStrLn f "module Plugin where foo = \"2\" :: Double"

      sym <- loadSymbolFromModule (defaultEnv { importPaths=[tmpDir] }) "Plugin" "foo" ::
                IO (EitherSymbol Double)
      case sym of
        Right _  -> assertFailure "unexpected right value"
        Left msg -> assertContains
                      "Couldn't match expected type `Double' with actual type\
                       \ `[Char]'"
                      msg

case_safe_haskell_is_enforced :: IO ()
case_safe_haskell_is_enforced =
    withSystemTempDirectory "test." $ \tmpDir -> do
      let code = "module Plugin where\n\
                 \import System.IO.Unsafe (unsafePerformIO)\n\
                 \import System.IO\n\
                 \fun :: Double -> Double\n\
                 \fun a = unsafePerformIO $ do {print a; return (a*2)}\n"

      withFile (joinPath [tmpDir, "Plugin.hs"]) WriteMode $ \f ->
        hPutStrLn f code
      sym <- loadSymbolFromModule (defaultEnv { importPaths=[tmpDir] })
                        "Plugin" "fun" :: IO (EitherSymbol (Double -> Double))
      case sym of
        Right _  -> assertFailure "unexpected right value"
        Left msg -> assertContains "The module itself isn't safe." msg

assertContains :: ByteString -> ByteString -> Assertion
assertContains inf s =
    assertBool ("'" ++ unpack s ++ "' does not contain '" ++ unpack s ++ "'")
               (isInfixOf inf s)

assertIsRight :: EitherSymbol a -> Assertion
assertIsRight v = case v of
                    Right _ -> return ()
                    Left msg -> assertFailure (unpack msg)
