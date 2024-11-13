{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Main where

import Api.Ledger.Create (CreateLedgerOptions (..), createLedger)
import Api.Ledger.Transactions.Create (
  CreateTransactionOptions (..),
  LedgerErrResponse,
  TransactionsData,
  createTransaction,
 )
import Control.Concurrent.Async (concurrently)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Numscript.Format (format)
import Numscript.Gen (generateProgram, generateSeeds)
import Prelude hiding (putStrLn)

runOnPort :: Text -> Text -> Int -> IO (Either LedgerErrResponse TransactionsData)
runOnPort seedProgram program port_ = do
  uuid <- UUID.toASCIIBytes <$> nextRandom

  createLedger
    CreateLedgerOptions
      { port = port_
      , ledgerName = uuid
      }

  _ <-
    createTransaction
      CreateTransactionOptions
        { port = port_
        , ledgerName = uuid
        , script = seedProgram
        }

  createTransaction
    CreateTransactionOptions
      { port = port_
      , ledgerName = uuid
      , script = program
      }

data OkReason
  = BothErr
  | CompileErr
  | Same
  deriving (Show)

runOnce :: IO (Either () OkReason)
runOnce = do
  seedProgram <- Numscript.Format.format <$> generateSeeds
  program <- Numscript.Format.format <$> generateProgram
  let runOnPort_ = runOnPort seedProgram program

  (legacyImpl, rewriteImpl) <- concurrently (runOnPort_ 3068) (runOnPort_ 3069)
  case (legacyImpl, rewriteImpl) of
    (Left _, Left _) -> return $ Right BothErr
    -- TODO check there is a compile err
    (Left _, Right _) -> return $ Right CompileErr
    _ | legacyImpl == rewriteImpl -> return $ Right Same
    _ -> do
      putStrLn "Got mismatch:"
      putStrLn "--- Seed: "
      putStrLn seedProgram
      putStrLn "--- Script: "
      putStrLn program
      putStrLn "--- Legacy implementation:"
      print legacyImpl
      putStrLn "--- New implementation:"
      print rewriteImpl
      return $ Left ()

runTimes :: Int -> IO ()
runTimes n | n <= 0 = return ()
runTimes n = do
  e <- runOnce
  case e of
    Left () -> return ()
    Right reason -> do
      print reason
      runTimes $ n - 1

main :: IO ()
main = runTimes 10
