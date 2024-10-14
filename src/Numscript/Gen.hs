module Numscript.Gen where

import Control.Applicative (liftA3)
import Test.QuickCheck

data Tree
  = Empty
  | Node Tree Int Tree
  deriving (Eq, Show)

bt :: Gen Tree
bt =
  oneof
    [ return Empty
    , liftA3 Node bt arbitrary bt
    ]

main :: IO ()
main = do
  x <- generate bt
  print x
