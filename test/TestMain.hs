module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified TestFormat
import qualified TestGen
import qualified TestPostings
import qualified TestUtils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ TestFormat.tests
    , TestGen.tests
    , TestPostings.tests
    , TestUtils.tests
    ]
