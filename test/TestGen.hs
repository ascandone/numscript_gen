module TestGen (tests) where

import Numscript.Gen (portionsList)
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "gen"
    [ QC.testProperty "portionList" $ do
        ps <- portionsList
        return $ sum ps == 1
    ]
