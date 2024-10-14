{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO (putStrLn)
import Numscript.Format (format)
import Numscript.Gen (generateProgram)
import Prelude hiding (putStrLn)

main :: IO ()
main = do
  putStrLn "generating.."
  program <- generateProgram
  let fmt = format program
  putStrLn fmt
