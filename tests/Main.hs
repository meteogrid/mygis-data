import Test.Framework (defaultMain)

import qualified TestContext
import qualified TestRasterIO


main :: IO ()
main = defaultMain [
    TestContext.tests
  , TestRasterIO.tests
  ]
