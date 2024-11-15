{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TestFormat (tests) where

import Data.Ratio ((%))
import Data.Text (Text)
import Numscript (AllotmentClause (..), Destination (..), KeptOrDest (..), Monetary (..), Program, Source (..), Statement (..), from)
import qualified Numscript.Format
import Test.Tasty
import Test.Tasty.HUnit

data TestCase = TestCase
  { label :: String
  , program :: Program
  , out :: Text
  }

fmtTests :: [TestCase]
fmtTests =
  [ TestCase
      { label = "simple send"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source = "src"
              , destination = "dest"
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = @src\n\
          \  destination = @dest\n\
          \)"
      }
  , TestCase
      { label = "send*"
      , program =
          [ SendAll
              { asset = "USD/2"
              , source = "src"
              , destination = "dest"
              }
          ]
      , out =
          "send [USD/2 *] (\n\
          \  source = @src\n\
          \  destination = @dest\n\
          \)"
      }
  , TestCase
      { label = "many statements"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source = "world"
              , destination = "src"
              }
          , Send
              { amount = Monetary "USD/2" 200
              , source = "src"
              , destination = "dest"
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = @world\n\
          \  destination = @src\n\
          \)\n\
          \\n\
          \send [USD/2 200] (\n\
          \  source = @src\n\
          \  destination = @dest\n\
          \)"
      }
  , TestCase
      { label = "inorder src"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source =
                  SrcInorder
                    [ "s1"
                    , "s2"
                    ]
              , destination = "dest"
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = {\n\
          \    @s1\n\
          \    @s2\n\
          \  }\n\
          \  destination = @dest\n\
          \)"
      }
  , TestCase
      { label = "nested inorder src"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source =
                  SrcInorder
                    [ "s1"
                    , SrcInorder
                        [ "a1"
                        , "a2"
                        ]
                    , "s2"
                    ]
              , destination = "dest"
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = {\n\
          \    @s1\n\
          \    {\n\
          \      @a1\n\
          \      @a2\n\
          \    }\n\
          \    @s2\n\
          \  }\n\
          \  destination = @dest\n\
          \)"
      }
  , TestCase
      { label = "capped src"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source =
                  SrcCapped (Monetary "USD/2" 42) "src"
              , destination = "dest"
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = max [USD/2 42] from @src\n\
          \  destination = @dest\n\
          \)"
      }
  , TestCase
      { label = "unbounded overdraft src"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source = SrcAccountOverdraft "src" Nothing
              , destination = "dest"
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = @src allowing unbounded overdraft\n\
          \  destination = @dest\n\
          \)"
      }
  , TestCase
      { label = "bounded overdraft src"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source = SrcAccountOverdraft "src" $ Just $ Monetary "COIN" 200
              , destination = "dest"
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = @src allowing overdraft up to [COIN 200]\n\
          \  destination = @dest\n\
          \)"
      }
  , TestCase
      { label = "allotment src"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source =
                  SrcAllotment
                    [ (1 % 3) `from` "s1"
                    , (2 % 3) `from` "s2"
                    ]
              , destination = "dest"
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = {\n\
          \    1/3 from @s1\n\
          \    2/3 from @s2\n\
          \  }\n\
          \  destination = @dest\n\
          \)"
      }
  , TestCase
      { label = "dest inorder"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source = "src"
              , destination =
                  DestInorder
                    [ (Monetary "COIN" 1, To "d1")
                    , (Monetary "COIN" 2, Kept)
                    , (Monetary "COIN" 3, To "d2")
                    ]
                    (To "d3")
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = @src\n\
          \  destination = {\n\
          \    max [COIN 1] to @d1\n\
          \    max [COIN 2] kept\n\
          \    max [COIN 3] to @d2\n\
          \    remaining to @d3\n\
          \  }\n\
          \)"
      }
  , TestCase
      { label = "dest allotment"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source = "src"
              , destination =
                  DestAllotment
                    [ AllotmentClause (1 % 3) (To "d1")
                    , AllotmentClause (2 % 3) Kept
                    ]
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = @src\n\
          \  destination = {\n\
          \    1/3 to @d1\n\
          \    2/3 kept\n\
          \  }\n\
          \)"
      }
  , TestCase
      { label = "send all"
      , program =
          [ Send
              { amount = Monetary "USD/2" 100
              , source = "src"
              , destination =
                  DestAllotment
                    [ AllotmentClause (1 % 3) (To "d1")
                    , AllotmentClause (2 % 3) Kept
                    ]
              }
          ]
      , out =
          "send [USD/2 100] (\n\
          \  source = @src\n\
          \  destination = {\n\
          \    1/3 to @d1\n\
          \    2/3 kept\n\
          \  }\n\
          \)"
      }
  ]

tests :: TestTree
tests =
  testGroup
    "format"
    [ testCase (label t) $
      Numscript.Format.format (program t) @?= out t
    | t <- fmtTests
    ]
