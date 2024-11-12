{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Api.Ledger.Create (
    RequestData (..),
    createLedger,
) where

import Data.ByteString (ByteString)
import Data.Function ((&))
import Network.HTTP.Simple
import Prelude hiding (putStrLn)

data RequestData
    = RequestData
    { port :: Int
    , ledgerName :: ByteString
    }

-- | POST /v2/:ledger
createLedger :: RequestData -> IO ()
createLedger req = do
    _ <- httpNoBody $ buildRequest req
    return ()

buildRequest :: RequestData -> Request
buildRequest d =
    defaultRequest
        & setRequestMethod "POST"
        & setRequestPort d.port
        & setRequestPath ("/v2/" <> d.ledgerName)
