{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Main where

import Api.Ledger.Create (CreateLedgerOptions (..), createLedger)
import Api.Ledger.Transactions.Create (
  CreateTransactionOptions (..),
  LedgerErrResponse (..),
  TransactionsData (..),
  createTransaction,
  removeEmptyPostings,
 )
import Control.Concurrent.Async (concurrently)
import Data.List (isInfixOf)
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

data CompileErr
  = IsAlreadyEmpty
  | UnboundedSourceOnlyLast
  deriving (Show)

data OkReason
  = NotEnoughFunds
  | CompileErr
  | KnownCompileErr CompileErr
  | Same
  deriving (Show)

runOnce :: IO (Either () OkReason)
runOnce = do
  seedProgram <- Numscript.Format.format <$> generateSeeds
  program <- Numscript.Format.format <$> generateProgram
  let runOnPort' = runOnPort seedProgram program
  (legacyImpl, rewriteImpl) <- concurrently (runOnPort' 3068) (runOnPort' 3069)
  case (legacyImpl, rewriteImpl) of
    (Left ErrResponse{errorCode = "INSUFFICIENT_FUND"}, Left ErrResponse{errorCode = "INTERPRETER_RUNTIME"}) -> return $ Right NotEnoughFunds
    (Left ErrResponse{errorCode = "COMPILATION_FAILED", errorMessage = e}, _) | "is already empty" `isInfixOf` e -> return $ Right (KnownCompileErr IsAlreadyEmpty)
    (Left ErrResponse{errorCode = "COMPILATION_FAILED", errorMessage = e}, _) | "an unbounded subsource can only be in last position" `isInfixOf` e -> return $ Right (KnownCompileErr UnboundedSourceOnlyLast)
    (Left ErrResponse{errorCode = "COMPILATION_FAILED"}, _) -> return $ Right CompileErr
    (Right p1, Right p2) | removeEmptyPostings p1.postings == removeEmptyPostings p2.postings -> return $ Right Same
    _ -> do
      putStrLn "❌ Got mismatch:"
      putStrLn "//----- Seed: "
      putStrLn seedProgram
      putStrLn "//----- Script: "
      putStrLn program
      putStrLn "//----- Legacy implementation:"
      print legacyImpl
      putStrLn "//----- New implementation:"
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

generateScript :: IO ()
generateScript = do
  p <- generateProgram
  putStrLn $ Numscript.Format.format p
  return ()
