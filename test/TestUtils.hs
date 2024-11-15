{-# LANGUAGE OverloadedStrings #-}

module TestUtils (tests) where

import Numscript
import Numscript.Utils (cleanupNumscript)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Postings"
        [ testCase "keep world in last position" $
            let src =
                    Send
                        { amount = Monetary "COIN" 42
                        , source =
                            SrcInorder
                                [ "a"
                                , "b"
                                , "world"
                                ]
                        , destination = "dest"
                        }
             in cleanupNumscript [src] @?= [src]
        , testCase "keep unbounded src in last position" $
            let src =
                    Send
                        { amount = Monetary "COIN" 42
                        , source =
                            SrcInorder
                                [ "a"
                                , "b"
                                , SrcAccountOverdraft "c" Nothing
                                ]
                        , destination = "dest"
                        }
             in cleanupNumscript [src] @?= [src]
        , testCase "keep bounded src" $
            let src =
                    Send
                        { amount = Monetary "COIN" 42
                        , source =
                            SrcInorder
                                [ "a"
                                , "b"
                                , SrcAccountOverdraft "c" (Just $ Monetary "COIN" 10)
                                , "z"
                                ]
                        , destination = "dest"
                        }
             in cleanupNumscript [src] @?= [src]
        , testCase "remove world not in last position" $
            cleanupNumscript
                [ Send
                    { amount = Monetary "COIN" 42
                    , source =
                        SrcInorder
                            [ "world"
                            , "a"
                            , "world"
                            , "b"
                            ]
                    , destination = "dest"
                    }
                ]
                @?= [ Send
                        { amount = Monetary "COIN" 42
                        , source =
                            SrcInorder
                                [ "a"
                                , "b"
                                ]
                        , destination = "dest"
                        }
                    ]
        , testCase "remove overdraft not in last position" $
            cleanupNumscript
                [ Send
                    { amount = Monetary "COIN" 42
                    , source =
                        SrcInorder
                            [ SrcAccountOverdraft "x" Nothing
                            , "a"
                            , SrcAccountOverdraft "y" Nothing
                            , "b"
                            ]
                    , destination = "dest"
                    }
                ]
                @?= [ Send
                        { amount = Monetary "COIN" 42
                        , source =
                            SrcInorder
                                [ "a"
                                , "b"
                                ]
                        , destination = "dest"
                        }
                    ]
        , testCase "apply in nested sources" $
            cleanupNumscript
                [ Send
                    { amount = Monetary "COIN" 42
                    , source =
                        SrcInorder
                            [ "a"
                            , "b"
                            , SrcInorder
                                [ "world"
                                , "z"
                                ]
                            ]
                    , destination = "dest"
                    }
                ]
                @?= [ Send
                        { amount = Monetary "COIN" 42
                        , source =
                            SrcInorder
                                [ "a"
                                , "b"
                                , SrcInorder
                                    [ "z"
                                    ]
                                ]
                        , destination = "dest"
                        }
                    ]
        , testCase "apply nested in allotments" $
            cleanupNumscript
                [ Send
                    { amount = Monetary "COIN" 42
                    , source =
                        SrcAllotment
                            [ AllotmentClause 1 $
                                SrcInorder
                                    [ SrcInorder
                                        [ "world"
                                        , "a"
                                        ]
                                    , "world"
                                    , "b"
                                    ]
                            ]
                    , destination = "dest"
                    }
                ]
                @?= [ Send
                        { amount = Monetary "COIN" 42
                        , source =
                            SrcAllotment
                                [ AllotmentClause 1 $
                                    SrcInorder
                                        [ SrcInorder
                                            [ "a"
                                            ]
                                        , "b"
                                        ]
                                ]
                        , destination = "dest"
                        }
                    ]
        , testCase "remove duplicates in inorder" $
            cleanupNumscript
                [ Send
                    { amount = Monetary "COIN" 42
                    , source =
                        SrcInorder
                            [ "a"
                            , "b"
                            , "a" -- "@a is already empty at this point"
                            ]
                    , destination = "dest"
                    }
                ]
                @?= [ Send
                        { amount = Monetary "COIN" 42
                        , source =
                            SrcInorder
                                [ "a"
                                , "b"
                                ]
                        , destination = "dest"
                        }
                    ]
                    -- , testCase "remove duplicates in inorder (nested)" $
                    --     cleanupNumscript
                    --         [ Send
                    --             { amount = Monetary "COIN" 42
                    --             , source =
                    --                 SrcInorder
                    --                     [ "a"
                    --                     , "b"
                    --                     , SrcInorder
                    --                         [ "c"
                    --                         , "a" -- "@a is already empty at this point"
                    --                         ]
                    --                     ]
                    --             , destination = "dest"
                    --             }
                    --         ]
                    --         @?= [ Send
                    --                 { amount = Monetary "COIN" 42
                    --                 , source =
                    --                     SrcInorder
                    --                         [ "a"
                    --                         , "b"
                    --                         , SrcInorder
                    --                             [ "c"
                    --                             ]
                    --                         ]
                    --                 , destination = "dest"
                    --                 }
                    --             ]
        ]
