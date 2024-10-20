module Main where

import Data.Text.IO (putStrLn)
import Numscript.Format (format)
import Numscript.Gen (generateProgram)
import Prelude hiding (putStrLn)

main :: IO ()
main = do
  program <- generateProgram
  let fmt = format program
  putStrLn fmt
