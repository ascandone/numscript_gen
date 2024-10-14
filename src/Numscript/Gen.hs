{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Numscript.Gen (program, generateProgram, Portions (..)) where

import Control.Monad (replicateM)
import Data.Ratio ((%))
import qualified Data.Ratio as Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Numscript
import Test.QuickCheck

isNonNegative :: (Ord a, Num a) => a -> Bool
isNonNegative x = x >= 0

isPositive :: (Ord a, Num a) => a -> Bool
isPositive x = x > 0

toText :: (Show a) => a -> Text
toText = T.pack . show

nonEmptyVectorOf :: Int -> Gen a -> Gen [a]
nonEmptyVectorOf len g = do
  s <- choose (1, len)
  replicateM s g

newtype Portions = Portions [Rational] deriving (Show)

portionsUpTo :: Rational -> Int -> Gen [Rational]
portionsUpTo _ s | s <= 0 = return []
portionsUpTo r 1 = return [r]
portionsUpTo 0 _ = return []
portionsUpTo r s = do
  p <- portionUpTo r
  ps <- portionsUpTo (r - 1) (s - 1)
  return (p : ps)

portionUpTo :: Rational -> Gen Rational
portionUpTo r = do
  num <- choose (1, Ratio.numerator r)
  return $ num % Ratio.denominator r

instance Arbitrary Portions where
  arbitrary :: Gen Portions
  arbitrary = sized (fmap Portions . portionsUpTo 1 . (+ 1))

monetary :: Gen Numscript.Monetary
monetary = do
  Numscript.Monetary "COIN" <$> arbitrary `suchThat` isNonNegative

portion :: Gen Rational
portion = do
  den <- arbitrary `suchThat` isPositive
  num <- choose (0, den)
  return $ num % den

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

source :: Int -> Gen Numscript.Source
source s =
  oneof $
    if s <= 0
      then base
      else base ++ rec_
 where
  base =
    [ return Numscript.SrcAccount
        <*> account
    , return Numscript.SrcAccountOverdraft
        <*> account
        <*> overdraft
    , return Numscript.SrcCapped
        <*> monetary
        <*> source (s - 1)
    ]
  rec_ =
    [ return Numscript.SrcInorder
        <*> nonEmptyVectorOf s (source (s - 1))
    , return Numscript.SrcAllotment
        <*> nonEmptyVectorOf s (allotmentClause (source $ s - 1))
    ]

allotmentClause :: Gen a -> Gen (Numscript.AllotmentClause a)
allotmentClause x =
  return Numscript.AllotmentClause
    <*> portion
    <*> x

destination :: Int -> Gen Numscript.Destination
destination s =
  oneof $
    if s <= 0
      then base
      else base ++ rec_
 where
  base =
    [ return $ Numscript.DestAccount "addr"
    ]
  rec_ =
    [ return Numscript.DestInorder
        <*> nonEmptyVectorOf s (destinationInorderClause (s - 1))
        <*> keptOrDest (s - 1)
    , return Numscript.DestAllotment
        <*> nonEmptyVectorOf s (allotmentClause (keptOrDest $ s - 1))
    ]

destinationInorderClause :: Int -> Gen (Numscript.Monetary, Numscript.KeptOrDest)
destinationInorderClause s =
  return (,)
    <*> monetary
    <*> keptOrDest s

keptOrDest :: Int -> Gen Numscript.KeptOrDest
keptOrDest s =
  oneof
    [ return Numscript.Kept
    , return Numscript.To
        <*> destination s
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
