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
  deriving (Eq, Show)

from :: Rational -> Source -> AllotmentClause Source
from = AllotmentClause

data Source
  = SrcAccount Text
  | SrcAccountOverdraft Text (Maybe Monetary)
  | SrcCapped Monetary Source
  | SrcInorder [Source]
  | SrcAllotment [AllotmentClause Source]
  deriving (Eq, Show)

data CappedDestination
  = CappedDestination Monetary KeptOrDest
  deriving (Eq, Show)

data Destination
  = DestAccount Text
  | DestInorder [(Monetary, KeptOrDest)] KeptOrDest
  | DestAllotment [AllotmentClause KeptOrDest]
  deriving (Eq, Show)

data KeptOrDest
  = Kept
  | To Destination
  deriving (Eq, Show)

data Monetary = Monetary Text Integer
  deriving (Eq, Show)

data Statement
  = Send
  { amount :: Monetary
  , source :: Source
  , destination :: Destination
  }
  deriving (Eq, Show)

type Program = [Statement]

instance IsString Source where
  fromString :: String -> Source
  fromString = SrcAccount . T.pack

instance IsString Destination where
  fromString :: String -> Destination
  fromString = DestAccount . T.pack
