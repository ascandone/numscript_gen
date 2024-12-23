{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Api.Ledger.Transactions.Create (
    CreateTransactionOptions (..),
    createTransaction,
    LedgerErrResponse (..),
    TransactionsData (..),
    Posting (..),
    normalizePostings,
    removeEmptyPostings,
) where

import Control.Applicative ((<|>))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Function ((&))
import qualified Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Prelude hiding (putStrLn)

data CreateTransactionOptions
    = CreateTransactionOptions
    { port :: Int
    , ledgerName :: ByteString
    , script :: Text
    }

-- | POST /v2/:ledger/transactions
createTransaction :: CreateTransactionOptions -> IO (Either LedgerErrResponse TransactionsData)
createTransaction req = do
    response <- httpJSON $ buildRequest req
    let (LedgerResponse e) = getResponseBody response
    return $ data_ <$> e

buildRequest :: CreateTransactionOptions -> Request
buildRequest d =
    defaultRequest
        & setRequestMethod "POST"
        & setRequestPort d.port
        & setRequestPath ("/v2/" <> d.ledgerName <> "/transactions")
        & setRequestBodyJSON body
  where
    script_ =
        Aeson.object
            [ "plain" .= d.script
            ]

    body =
        Aeson.object
            [ "script" .= script_
            ]

data LedgerErrResponse
    = ErrResponse
    { errorCode :: String
    , errorMessage :: String
    }
    deriving (Generic, Show, Eq)

instance Aeson.FromJSON LedgerErrResponse

newtype LedgerOkResponse
    = OkResponse
    { data_ :: TransactionsData
    }
    deriving (Generic, Show, Eq)

data Posting
    = Posting
    { source :: Text
    , destination :: Text
    , amount :: Integer
    , asset :: Text
    }
    deriving (Generic, Show, Eq)

instance Aeson.FromJSON Posting where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

newtype TransactionsData
    = TransactionsData
    { postings :: [Posting]
    }
    deriving (Generic, Eq)

instance Show TransactionsData where
    show td = header <> "\n" <> T.unpack body
      where
        header = "source,destination,amount,asset"
        body = T.unlines $ map showPosting td.postings
        showPosting p@Posting{} =
            let amt = T.pack $ show p.amount
             in p.source <> "," <> p.destination <> "," <> amt <> "," <> p.asset

instance Aeson.FromJSON TransactionsData where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance Aeson.FromJSON LedgerOkResponse where
    parseJSON =
        Aeson.genericParseJSON
            Aeson.defaultOptions
                { Aeson.fieldLabelModifier = keywordFieldLabelModifier
                }

keywordFieldLabelModifier :: String -> String
keywordFieldLabelModifier "data_" = "data"
keywordFieldLabelModifier x = x

newtype LedgerResponse
    = LedgerResponse (Either LedgerErrResponse LedgerOkResponse)
    deriving (Show)

instance Aeson.FromJSON LedgerResponse where
    parseJSON x = LedgerResponse <$> e
      where
        e = Right <$> Aeson.parseJSON x <|> Left <$> Aeson.parseJSON x

normalizePostings :: [Posting] -> Map (Text, Text) Integer
normalizePostings =
    Data.List.foldl'
        (\oldMap posting -> Map.alter (f posting.amount) (posting.source, posting.destination) oldMap)
        Map.empty
  where
    f 0 oldValue = oldValue
    f amt oldValue = case oldValue of
        Nothing -> Just amt
        Just oldAmt -> Just $ amt + oldAmt

removeEmptyPostings :: [Posting] -> [Posting]
removeEmptyPostings = filter (\p -> p.amount /= 0)
