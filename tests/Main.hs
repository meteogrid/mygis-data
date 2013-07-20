import Test.Framework (defaultMain)

import qualified TestContext
import qualified TestRasterIO
import qualified TestUnits


main :: IO ()
main = defaultMain [
    TestContext.tests
  , TestRasterIO.tests
  , TestUnits.tests
  ]
