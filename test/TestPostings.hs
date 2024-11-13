{-# LANGUAGE OverloadedStrings #-}

module TestPostings (tests) where

import Api.Ledger.Transactions.Create (Posting (..), normalizePostings)
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Postings"
        [ testCase "insert new postings" $
            normalizePostings
                [ Posting
                    { source = "src"
                    , destination = "dest"
                    , amount = 100
                    , asset = "COIN"
                    }
                ]
                @?= Map.fromList
                    [ (("src", "dest"), 100)
                    ]
        , testCase "update previous postings" $
            normalizePostings
                [ Posting
                    { source = "src"
                    , destination = "dest"
                    , amount = 10
                    , asset = "COIN"
                    }
                , Posting
                    { source = "src"
                    , destination = "dest"
                    , amount = 20
                    , asset = "COIN"
                    }
                ]
                @?= Map.fromList
                    [ (("src", "dest"), 30)
                    ]
        , testCase "complex posting" $
            normalizePostings
                [ Posting
                    { source = "src"
                    , destination = "dest"
                    , amount = 10
                    , asset = "COIN"
                    }
                , Posting
                    { source = "src2"
                    , destination = "dest2"
                    , amount = 1
                    , asset = "COIN"
                    }
                , Posting
                    { source = "dest"
                    , destination = "src"
                    , amount = 2
                    , asset = "COIN"
                    }
                , Posting
                    { source = "src"
                    , destination = "dest"
                    , amount = 20
                    , asset = "COIN"
                    }
                ]
                @?= Map.fromList
                    [ (("src", "dest"), 30)
                    , (("src2", "dest2"), 1)
                    , (("dest", "src"), 2)
                    ]
        , testCase "skip zero postings" $
            normalizePostings
                [ Posting
                    { source = "src"
                    , destination = "dest"
                    , amount = 0
                    , asset = "COIN"
                    }
                ]
                @?= Map.empty
        ]
