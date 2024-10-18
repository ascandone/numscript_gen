{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Numscript.Gen (
  program,
  generateProgram,
  portionUpTo,
  portionsUpTo,
) where

import Control.Monad (forM, replicateM)
import Data.Ratio ((%))
import qualified Data.Ratio as Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Numscript
import Test.QuickCheck

toText :: (Show a) => a -> Text
toText = T.pack . show

nonEmptyVectorOf :: Int -> Gen a -> Gen [a]
nonEmptyVectorOf len g = do
  s <- choose (1, len)
  replicateM s g

-- | a list of rationals that sums to r, and has at most len s
portionsUpTo :: Rational -> Int -> Gen [Rational]
portionsUpTo _ s | s <= 0 = return []
portionsUpTo r 1 = return [r]
portionsUpTo 0 _ = return []
portionsUpTo r s = do
  p <- portionUpTo r
  ps <- portionsUpTo (r - p) (s - 1)
  return (p : ps)

-- A rational that is at most r
portionUpTo :: Rational -> Gen Rational
portionUpTo r | r <= 0 = error "Expected input to be positive"
portionUpTo r | r > 1 = error "Expected input to be at most 1"
portionUpTo r = do
  Positive mult <- arbitrary
  let num = mult * Ratio.numerator r
  let den = mult * Ratio.denominator r
  num' <- choose (1, num)
  return $ num' % den

monetary :: Gen Numscript.Monetary
monetary = do
  (NonNegative amt) <- arbitrary
  return $ Numscript.Monetary "COIN" amt

overdraft :: Gen (Maybe Numscript.Monetary)
overdraft =
  oneof
    [ return Nothing
    , Just <$> monetary
    ]

account :: Gen Text
account = do
  k <- choose (0 :: Int, 5)
  return $ "acc" <> toText k

zeroFreqIf :: (Num p) => p -> Bool -> p
zeroFreqIf x b = if b then 0 else x

source :: Int -> Gen Numscript.Source
source s =
  frequency
    [
      ( 15
      , return Numscript.SrcAccount
          <*> account
      )
    ,
      ( 10
      , return Numscript.SrcAccountOverdraft
          <*> account
          <*> overdraft
      )
    ,
      ( 5
      , return Numscript.SrcCapped
          <*> monetary
          <*> source (s - 1)
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.SrcInorder
          <*> nonEmptyVectorOf s (source (s - 1))
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.SrcAllotment
          <*> allotmentClauses (source (s - 1))
      )
    ]
 where
  stopRecursion = s <= 0

allotmentClauses :: Gen a -> Gen [Numscript.AllotmentClause a]
allotmentClauses gen = sized $ \size -> do
  let posSize = size + 1
  portions <- portionsUpTo 1 posSize
  forM portions $ \rat ->
    return (Numscript.AllotmentClause rat)
      <*> gen

destination :: Int -> Gen Numscript.Destination
destination s =
  frequency
    [
      ( 15
      , return Numscript.DestAccount
          <*> account
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.DestInorder
          <*> nonEmptyVectorOf s (destinationInorderClause (s - 1))
          <*> keptOrDest (s - 1)
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.DestAllotment
          <*> allotmentClauses (keptOrDest (s - 1))
      )
    ]
 where
  stopRecursion = s <= 0

destinationInorderClause :: Int -> Gen (Numscript.Monetary, Numscript.KeptOrDest)
destinationInorderClause s =
  return (,)
    <*> monetary
    <*> keptOrDest s

keptOrDest :: Int -> Gen Numscript.KeptOrDest
keptOrDest s =
  frequency
    [
      ( 1
      , return Numscript.Kept
      )
    ,
      ( 3
      , return Numscript.To
          <*> destination s
      )
    ]

statement :: Gen Numscript.Statement
statement = do
  amt <- monetary
  src <- source 4
  dest <- destination 4
  return $
    Numscript.Send
      { Numscript.amount = amt
      , Numscript.source = src
      , Numscript.destination = dest
      }

program :: Gen Numscript.Program
program = listOf statement

generateProgram :: IO Numscript.Program
generateProgram = generate program
