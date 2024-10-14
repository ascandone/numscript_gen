{-# LANGUAGE InstanceSigs #-}

module Numscript (
  Source (..),
  Destination (..),
  AllotmentClause (..),
  KeptOrDest (..),
  Statement (..),
  Program,
  Monetary (..),
  CappedDestination (..),
  from,
) where

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T

data AllotmentClause a
  = AllotmentClause Rational a

from :: Rational -> Source -> AllotmentClause Source
from = AllotmentClause

data Source
  = SrcAccount Text
  | SrcAccountOverdraft Text (Maybe Monetary)
  | SrcCapped Monetary Source
  | SrcInorder [Source]
  | SrcAllotment [AllotmentClause Source]

data CappedDestination
  = CappedDestination Monetary KeptOrDest

data Destination
  = DestAccount Text
  | DestInorder [(Monetary, KeptOrDest)] KeptOrDest
  | DestAllotment [AllotmentClause KeptOrDest]

data KeptOrDest
  = Kept
  | To Destination

data Monetary = Monetary Text Integer

data Statement
  = Send
  { amount :: Monetary
  , source :: Source
  , destination :: Destination
  }

type Program = [Statement]

instance IsString Source where
  fromString :: String -> Source
  fromString = SrcAccount . T.pack

instance IsString Destination where
  fromString :: String -> Destination
  fromString = DestAccount . T.pack
