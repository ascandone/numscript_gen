{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Numscript.Gen (
  program,
  generateProgram,
  portionsList,
) where

import Control.Monad (forM)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Numscript
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

toText :: (Show a) => a -> Text
toText = T.pack . show

portionsList :: Gen [Rational]
portionsList = do
  xs <- QC.listOf1 $ do
    QC.Positive x <- QC.arbitrary
    return (x :: Integer)
  let total = sum xs
  return $ [x % total | x <- xs]

monetary :: Gen Numscript.Monetary
monetary = do
  (QC.NonNegative amt) <- QC.arbitrary
  return $ Numscript.Monetary "COIN" amt

overdraft :: Gen (Maybe Numscript.Monetary)
overdraft =
  QC.oneof
    [ return Nothing
    , Just <$> monetary
    ]

account :: Gen Text
account = do
  k <- QC.choose (0 :: Int, 5)
  return $ "acc" <> toText k

zeroFreqIf :: (Num p) => p -> Bool -> p
zeroFreqIf x b = if b then 0 else x

source :: Numscript.Monetary -> Int -> Gen Numscript.Source
source sent@(Numscript.Monetary _ sentAmt) s =
  QC.frequency
    [
      ( 5
      , return $ Numscript.SrcAccount "world"
      )
    ,
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
          <*> fmap (addMonetary sentAmt) monetary
          <*> source sent (s - 1)
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.SrcInorder
          <*> QC.listOf1 (source sent (s - 1))
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.SrcAllotment
          <*> allotmentClauses (source sent (s - 1))
      )
    ]
 where
  stopRecursion = s <= 0

allotmentClauses :: Gen a -> Gen [Numscript.AllotmentClause a]
allotmentClauses gen = do
  portions <- portionsList
  forM portions $ \rat ->
    return (Numscript.AllotmentClause rat)
      <*> gen

destination :: Int -> Gen Numscript.Destination
destination s =
  QC.frequency
    [
      ( 30
      , return Numscript.DestAccount
          <*> account
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.DestInorder
          <*> QC.listOf1 (destinationInorderClause (s - 1))
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
  QC.frequency
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
  sent <- monetary
  src <- source sent 4
  dest <- destination 4
  return $
    Numscript.Send
      { Numscript.amount = sent
      , Numscript.source = src
      , Numscript.destination = dest
      }

program :: Gen Numscript.Program
program = QC.listOf statement

generateProgram :: IO Numscript.Program
generateProgram = QC.generate program

addMonetary :: Integer -> Numscript.Monetary -> Numscript.Monetary
addMonetary x (Numscript.Monetary mon y) = Numscript.Monetary mon (x + y)