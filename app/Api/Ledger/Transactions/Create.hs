{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Api.Ledger.Transactions.Create (
    RequestData (..),
    makeTransaction,
    LedgerErrResponse (..),
    LedgerOkResponse (..),
    LedgerResponse (..),
    TransactionsData (..),
    Posting (..),
) where

import Control.Applicative ((<|>))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Prelude hiding (putStrLn)

data RequestData
    = RequestData
    { port :: Int
    , ledgerName :: ByteString
    , script :: Text
    }

-- | POST /v2/:ledger/transactions
makeTransaction :: RequestData -> IO (Either LedgerErrResponse TransactionsData)
makeTransaction req = do
    response <- httpJSON $ buildRequest req
    let (LedgerResponse e) = getResponseBody response
    return $ data_ <$> e

buildRequest :: RequestData -> Request
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
    deriving (Generic, Show)

instance Aeson.FromJSON LedgerErrResponse

newtype LedgerOkResponse
    = OkResponse
    { data_ :: TransactionsData
    }
    deriving (Generic, Show)

data Posting
    = Posting
    { source :: Text
    , destination :: Text
    , amount :: Integer
    , asset :: Text
    }
    deriving (Generic, Show)

instance Aeson.FromJSON Posting where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

newtype TransactionsData
    = TransactionsData
    { postings :: [Posting]
    }
    deriving (Generic, Show)

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
