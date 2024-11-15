{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Numscript.Gen (
  program,
  generateProgram,
  generateSeeds,
  portionsList,
) where

import Control.Monad (forM)
import Data.Ratio (Ratio, (%))
import qualified Data.Ratio as Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Numscript
import Numscript.Utils (cleanupNumscript)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

{- |  Pick an unbounded integer using a non uniform distribution

    Takes the odds "a/b" of extracting the given number,
    otherwise function is called recursively with "a+1/b+1"

    Example sample of 50 picks with rat=1/10 and n=0: [3,2,3,3,4,0,4,0,4,3,0,4,3,1,5,2,0,1,2,2,5,5,4,2,1,1,4,1,7,3,4,2,1,2,0,2,0,5,0,0,0,2,1,4,4,4,6,1,0,1]
-}
nonUniform :: (Enum n) => Ratio Int -> n -> Gen n
nonUniform rat _ | rat > 1 = error "rat must be <= 1"
nonUniform rat n =
  QC.frequency
    [ (numerator, return n)
    , (denominator - numerator, nonUniform nextRat (succ n))
    ]
 where
  numerator = Ratio.numerator rat
  denominator = Ratio.denominator rat
  nextRat = (numerator + 1) % (denominator + 1)

nonUniformListOf :: Gen a -> Gen [a]
nonUniformListOf g = do
  n <- nonUniform (1 % 10) 1
  QC.vectorOf n g

toText :: (Show a) => a -> Text
toText = T.pack . show

portionsList :: Gen [Rational]
portionsList = do
  xs <- nonUniformListOf $ do
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

data SourceOptions
  = SourceOptions
  { sentAmt :: Integer
  , maxNestingRemaining :: Int
  , isToplevel :: Bool
  }

defaultSourceOptions :: Numscript.Monetary -> Int -> SourceOptions
defaultSourceOptions (Numscript.Monetary _ amt) depth =
  SourceOptions
    { sentAmt = amt
    , maxNestingRemaining = depth
    , isToplevel = True
    }

sourceWith :: SourceOptions -> Gen Numscript.Source
sourceWith opts =
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
          <*> fmap (addMonetary opts.sentAmt) monetary
          <*> nestedSource
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.SrcInorder
          <*> nonUniformListOf nestedSource
      )
    ,
      ( 15 `zeroFreqIf` (stopRecursion || not opts.isToplevel)
      , return Numscript.SrcAllotment
          <*> allotmentClauses nestedSource
      )
    ]
 where
  nestedSource =
    sourceWith
      opts
        { isToplevel = False
        , maxNestingRemaining = opts.maxNestingRemaining - 1
        }
  stopRecursion = opts.maxNestingRemaining <= 0

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
          <*> nonUniformListOf (destinationInorderClause (s - 1))
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
  srcDepth <- nonUniform (1 % 10) 0
  destDepth <- nonUniform (1 % 10) 0

  sent <- monetary
  src <- sourceWith $ defaultSourceOptions sent srcDepth
  dest <- destination destDepth
  return $
    Numscript.Send
      { Numscript.amount = sent
      , Numscript.source = src
      , Numscript.destination = dest
      }

program :: Gen Numscript.Program
program = nonUniformListOf statement

generateProgram :: IO Numscript.Program
generateProgram = cleanupNumscript <$> QC.generate program

addMonetary :: Integer -> Numscript.Monetary -> Numscript.Monetary
addMonetary x (Numscript.Monetary mon y) = Numscript.Monetary mon (x + y)

-- | Generate a bunch of postings from @world to @acc_i, in order to seed the script
seeds :: Gen Numscript.Program
seeds = do
  forM [0 :: Int .. 5] $ \i -> do
    let index = T.pack $ show i
    amt <- monetary
    return $
      Numscript.Send
        { Numscript.amount = amt
        , Numscript.source = Numscript.SrcAccount "world"
        , Numscript.destination = Numscript.DestAccount $ "acc" <> index
        }

generateSeeds :: IO Numscript.Program
generateSeeds = QC.generate seeds