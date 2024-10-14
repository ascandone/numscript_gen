module TestGen (tests) where

import qualified Numscript.Gen
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "gen"
    [ QC.testProperty "generated portions sum to 1" $
        \(Numscript.Gen.Portions ps) ->
          sum ps == 1
    ]
