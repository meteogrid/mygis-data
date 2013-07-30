import Test.Framework (defaultMain)

import qualified TestGeoReference
import qualified TestRasterIO
import qualified TestUnits
import qualified TestGeneration
import qualified TestDynLoad


main :: IO ()
main = defaultMain [
    TestGeoReference.tests
  , TestRasterIO.tests
  , TestUnits.tests
  , TestGeneration.tests
  , TestDynLoad.tests
  ]
