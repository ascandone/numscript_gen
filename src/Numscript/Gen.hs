{-# LANGUAGE DuplicateRecordFields #-}
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

accountsNumber :: Int
accountsNumber = 15

smallerRat :: (Integral a) => Ratio a -> Ratio a
smallerRat rat = (Ratio.numerator rat + 1) % (Ratio.denominator rat + 1)

ratFrequency :: Ratio Int -> Gen a -> Gen a -> Gen a
ratFrequency rat _ _ | rat > 1 = error "rat must be <= 1"
ratFrequency rat x y =
  QC.frequency
    [ (numerator, x)
    , (denominator - numerator, y)
    ]
 where
  numerator = Ratio.numerator rat
  denominator = Ratio.denominator rat

{- |  Pick an unbounded integer using a non uniform distribution

    Takes the odds "a/b" of extracting the given number,
    otherwise function is called recursively with "a+1/b+1"

    Example sample of 50 picks with rat=1/10 and n=0: [3,2,3,3,4,0,4,0,4,3,0,4,3,1,5,2,0,1,2,2,5,5,4,2,1,1,4,1,7,3,4,2,1,2,0,2,0,5,0,0,0,2,1,4,4,4,6,1,0,1]
-}
nonUniform :: (Enum n) => Ratio Int -> n -> Gen n
nonUniform rat n = ratFrequency rat x y
 where
  x = return n
  y = nonUniform (smallerRat rat) (succ n)

-- | r likelyhood of picking True, 1-r of picking False
weightedCoin :: Ratio Int -> Gen Bool
weightedCoin r = ratFrequency r (return True) (return False)

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

account :: Gen Text
account = do
  k <- QC.choose (0 :: Int, accountsNumber)
  return $ "acc" <> toText k

zeroFreqIf :: (Num p) => p -> Bool -> p
zeroFreqIf x b = if b then 0 else x

data SourceOptions
  = SourceOptions
  { sentAmt :: Integer
  , isToplevel :: Bool
  , isUnbounded :: Bool
  , keepNestingProbabability :: Ratio Int
  }

defaultSourceOptions :: Numscript.Monetary -> Bool -> SourceOptions
defaultSourceOptions (Numscript.Monetary _ amt) unbounded =
  SourceOptions
    { sentAmt = amt
    , isToplevel = True
    , isUnbounded = unbounded
    , keepNestingProbabability = 1 % 15
    }

source :: SourceOptions -> Gen Numscript.Source
source opts = do
  stopRecursion <- weightedCoin opts.keepNestingProbabability
  QC.frequency
    [
      ( 5 `zeroFreqIf` opts.isUnbounded
      , return $ Numscript.SrcAccount "world"
      )
    ,
      ( 15
      , return Numscript.SrcAccount
          <*> account
      )
    ,
      ( 5
      , return Numscript.SrcAccountOverdraft
          <*> account
          <*> (Just <$> monetary)
      )
    ,
      ( 5 `zeroFreqIf` opts.isUnbounded
      , return Numscript.SrcAccountOverdraft
          <*> account
          <*> return Nothing
      )
    ,
      ( 5
      , return Numscript.SrcCapped
          <*> fmap (addMonetary opts.sentAmt) monetary
          <*> source (nestedSourceOpts{isUnbounded = False})
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.SrcInorder
          <*> nonUniformListOf (source nestedSourceOpts)
      )
    ,
      ( 15 `zeroFreqIf` (stopRecursion || not opts.isToplevel || opts.isUnbounded)
      , return Numscript.SrcAllotment
          <*> allotmentClauses (source nestedSourceOpts{isUnbounded = False})
      )
    ]
 where
  nestedSourceOpts =
    opts
      { isToplevel = False
      , keepNestingProbabability = smallerRat opts.keepNestingProbabability
      }

allotmentClauses :: Gen a -> Gen [Numscript.AllotmentClause a]
allotmentClauses gen = do
  portions <- portionsList
  forM portions $ \rat ->
    return (Numscript.AllotmentClause rat)
      <*> gen

newtype DestinationOptions
  = DestinationOptions
  { keepNestingProbabability :: Ratio Int
  }

defaultDestinationOptions :: DestinationOptions
defaultDestinationOptions =
  DestinationOptions
    { keepNestingProbabability = 1 % 15
    }

destination :: DestinationOptions -> Gen Numscript.Destination
destination opts = do
  stopRecursion <- weightedCoin opts.keepNestingProbabability
  QC.frequency
    [
      ( 30
      , return Numscript.DestAccount
          <*> account
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.DestInorder
          <*> nonUniformListOf (destinationInorderClause nestedDestOpts)
          <*> keptOrDest nestedDestOpts
      )
    ,
      ( 10 `zeroFreqIf` stopRecursion
      , return Numscript.DestAllotment
          <*> allotmentClauses (keptOrDest nestedDestOpts)
      )
    ]
 where
  nestedDestOpts =
    DestinationOptions
      { keepNestingProbabability = smallerRat opts.keepNestingProbabability
      }

destinationInorderClause :: DestinationOptions -> Gen (Numscript.Monetary, Numscript.KeptOrDest)
destinationInorderClause opts =
  return (,)
    <*> monetary
    <*> keptOrDest opts

keptOrDest :: DestinationOptions -> Gen Numscript.KeptOrDest
keptOrDest opts =
  QC.frequency
    [
      ( 1
      , return Numscript.Kept
      )
    ,
      ( 3
      , return Numscript.To
          <*> destination opts
      )
    ]

statement :: Gen Numscript.Statement
statement = do
  pickUnbounded <-
    QC.frequency
      [ (1, return True)
      , (3, return False)
      ]

  sent@(Numscript.Monetary asset _) <- monetary
  src <- source $ defaultSourceOptions sent pickUnbounded
  dest <- destination defaultDestinationOptions

  return $
    if pickUnbounded
      then
        Numscript.SendAll
          { Numscript.asset = asset
          , Numscript.source = src
          , Numscript.destination = dest
          }
      else
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
  forM [0 :: Int .. accountsNumber] $ \i -> do
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