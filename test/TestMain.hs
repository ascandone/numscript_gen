module Main (main) where

import qualified TestFormat
import qualified TestGen

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ TestFormat.tests
    , TestGen.tests
    ]
