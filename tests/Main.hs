import Test.Framework (defaultMain)

import qualified TestContext


main :: IO ()
main = defaultMain [
    TestContext.tests
    ]
