module TestGen (tests) where

import Data.Ratio ((%))
import qualified Numscript.Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (arbitrary)
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "gen"
    [ QC.testProperty "portionUpTo 1" $
        portionUpToProp 1
    , QC.testProperty "portionUpTo 1/2" $
        portionUpToProp (1 % 2)
    , QC.testProperty "portionUpTo 1/10" $
        portionUpToProp (1 % 10)
    , QC.testProperty "portionUpTo r" $ do
        (QC.Positive n) <- arbitrary
        (QC.Positive d) <- arbitrary
        return $ portionUpToProp (min n d % max n d)
    , QC.testProperty "sum to 1 with size 2" $
        sumProp 1 2
    , QC.testProperty "sum to 1/2 with size 2" $
        sumProp (1 % 2) 2
    ]

portionUpToProp :: Rational -> QC.Gen Bool
portionUpToProp r = do
  r' <- Numscript.Gen.portionUpTo r
  return $ r' > 0 && r' <= r

sumProp :: Rational -> Int -> QC.Gen Bool
sumProp r size = do
  ps <- Numscript.Gen.portionsUpTo r size
  return $ sum ps == r
