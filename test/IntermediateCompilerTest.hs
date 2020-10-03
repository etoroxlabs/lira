-- MIT License
-- 
-- Copyright (c) 2019 eToroX Labs
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module IntermediateCompilerTest
  ( tests
  )
where

import qualified Data.Map.Strict as Map

import           Lira.Contract
import           Lira.Contract.Intermediate
import           Lira.Contract.Parser
import           Lira.Backends.IntermediateCompiler

import           LiraParserTest hiding (tests)
import           LiraTestHelpers

import           Test.Hspec

tests :: Spec
tests = do
  canonicalNestedIfWithinTest
  activateMapSimple
  marginRefundMapSimple
  activateMapCanonicalIw
  refundMapCanonicalIw
  basicTransferTest
  timeTranslationIMemExpTest
  zeroContractCodeTest

canonicalNestedIfWithinTest :: Spec
canonicalNestedIfWithinTest = do
  it "canonical nested if-within" $ do
    intermediateCompile (parse' canonical_iw_source)
      `shouldBe` intermediateContract

 where
  intermediateContract =
    IntermediateContract parties transfers memExps activateMap marginRefundMap

  parties =
    [ Bound "0x1234567890123456789012345678901234567891"
    , Bound "0x1234567890123456789012345678901234567892"
    , Bound "0x1234567890123456789012345678901234567893"
    , Bound "0x1234567890123456789012345678901234567894"
    , Bound "0x1234567890123456789012345678901234567895"
    , Bound "0x1234567890123456789012345678901234567896"
    , Bound "0x1234567890123456789012345678901234567897"
    , Bound "0x1234567890123456789012345678901234567898"
    ]

  transfers =
    [ TransferCall
      { maxAmount    = 1
      , amount       = Lit (IntVal 1)
      , delay        = 0
      , tokenAddress = "0x1234567890123456789012345678901234567891"
      , from         = 0
      , to           = 0
      , memExpPath   = [(0, True), (1, True), (2, True)]
      }
    , TransferCall
      { maxAmount    = 1
      , amount       = Lit (IntVal 1)
      , delay        = 0
      , tokenAddress = "0x1234567890123456789012345678901234567892"
      , from         = 1
      , to           = 1
      , memExpPath   = [(0, True), (1, True), (2, False)]
      }
    , TransferCall
      { maxAmount    = 1
      , amount       = Lit (IntVal 1)
      , delay        = 0
      , tokenAddress = "0x1234567890123456789012345678901234567893"
      , from         = 2
      , to           = 2
      , memExpPath   = [(0, True), (1, False), (3, True), (4, True)]
      }
    , TransferCall
      { maxAmount    = 1
      , amount       = Lit (IntVal 1)
      , delay        = 0
      , tokenAddress = "0x1234567890123456789012345678901234567894"
      , from         = 3
      , to           = 3
      , memExpPath   = [(0, True), (1, False), (3, True), (4, False)]
      }
    , TransferCall
      { maxAmount    = 1
      , amount       = Lit (IntVal 1)
      , delay        = 0
      , tokenAddress = "0x1234567890123456789012345678901234567895"
      , from         = 4
      , to           = 4
      , memExpPath   = [(0, True), (1, False), (3, False)]
      }
    , TransferCall
      { maxAmount    = 1
      , amount       = Lit (IntVal 1)
      , delay        = 0
      , tokenAddress = "0x1234567890123456789012345678901234567896"
      , from         = 5
      , to           = 5
      , memExpPath   = [(0, False), (5, True)]
      }
    , TransferCall
      { maxAmount    = 1
      , amount       = Lit (IntVal 1)
      , delay        = 0
      , tokenAddress = "0x1234567890123456789012345678901234567897"
      , from         = 6
      , to           = 6
      , memExpPath   = [(0, False), (5, False), (6, True)]
      }
    , TransferCall
      { maxAmount    = 1
      , amount       = Lit (IntVal 1)
      , delay        = 0
      , tokenAddress = "0x1234567890123456789012345678901234567898"
      , from         = 7
      , to           = 7
      , memExpPath   = [(0, False), (5, False), (6, False)]
      }
    ]

  memExps =
    [ IMemExp
      { _IMemExpBegin = 0
      , _IMemExpEnd   = 1
      , _IMemExpIdent = 0
      , _IMemExp      = GtExp (MultExp (Lit (IntVal 1)) (Lit (IntVal 1)))
                              (Lit (IntVal 1))
      }
    , IMemExp
      { _IMemExpBegin = 0
      , _IMemExpEnd   = 2
      , _IMemExpIdent = 1
      , _IMemExp      = GtExp (MultExp (Lit (IntVal 2)) (Lit (IntVal 2)))
                              (Lit (IntVal 2))
      }
    , IMemExp
      { _IMemExpBegin = 0
      , _IMemExpEnd   = 3
      , _IMemExpIdent = 2
      , _IMemExp      = GtExp (MultExp (Lit (IntVal 3)) (Lit (IntVal 3)))
                              (Lit (IntVal 3))
      }
    , IMemExp
      { _IMemExpBegin = 0
      , _IMemExpEnd   = 4
      , _IMemExpIdent = 3
      , _IMemExp      = GtExp (MultExp (Lit (IntVal 4)) (Lit (IntVal 4)))
                              (Lit (IntVal 4))
      }
    , IMemExp
      { _IMemExpBegin = 0
      , _IMemExpEnd   = 5
      , _IMemExpIdent = 4
      , _IMemExp      = GtExp (MultExp (Lit (IntVal 5)) (Lit (IntVal 5)))
                              (Lit (IntVal 5))
      }
    , IMemExp
      { _IMemExpBegin = 0
      , _IMemExpEnd   = 6
      , _IMemExpIdent = 5
      , _IMemExp      = GtExp (MultExp (Lit (IntVal 6)) (Lit (IntVal 6)))
                              (Lit (IntVal 6))
      }
    , IMemExp
      { _IMemExpBegin = 0
      , _IMemExpEnd   = 7
      , _IMemExpIdent = 6
      , _IMemExp      = GtExp (MultExp (Lit (IntVal 7)) (Lit (IntVal 7)))
                              (Lit (IntVal 7))
      }
    ]

  activateMap = Map.fromList
    [ (("0x1234567890123456789012345678901234567891", 0), 1)
    , (("0x1234567890123456789012345678901234567892", 1), 1)
    , (("0x1234567890123456789012345678901234567893", 2), 1)
    , (("0x1234567890123456789012345678901234567894", 3), 1)
    , (("0x1234567890123456789012345678901234567895", 4), 1)
    , (("0x1234567890123456789012345678901234567896", 5), 1)
    , (("0x1234567890123456789012345678901234567897", 6), 1)
    , (("0x1234567890123456789012345678901234567898", 7), 1)
    ]

  marginRefundMap = Map.fromList
    [ ( [(0, False)]
      , [ ("0x1234567890123456789012345678901234567891", 0, 1)
        , ("0x1234567890123456789012345678901234567892", 1, 1)
        , ("0x1234567890123456789012345678901234567893", 2, 1)
        , ("0x1234567890123456789012345678901234567894", 3, 1)
        , ("0x1234567890123456789012345678901234567895", 4, 1)
        ]
      )
    , ( [(0, False), (5, False)]
      , [("0x1234567890123456789012345678901234567896", 5, 1)]
      )
    , ( [(0, False), (5, False), (6, False)]
      , [("0x1234567890123456789012345678901234567897", 6, 1)]
      )
    , ( [(0, False), (5, False), (6, True)]
      , [("0x1234567890123456789012345678901234567898", 7, 1)]
      )
    , ( [(0, False), (5, True)]
      , [ ("0x1234567890123456789012345678901234567897", 6, 1)
        , ("0x1234567890123456789012345678901234567898", 7, 1)
        ]
      )
    , ( [(0, True)]
      , [ ("0x1234567890123456789012345678901234567896", 5, 1)
        , ("0x1234567890123456789012345678901234567897", 6, 1)
        , ("0x1234567890123456789012345678901234567898", 7, 1)
        ]
      )
    , ( [(0, True), (1, False)]
      , [ ("0x1234567890123456789012345678901234567891", 0, 1)
        , ("0x1234567890123456789012345678901234567892", 1, 1)
        ]
      )
    , ( [(0, True), (1, False), (3, False)]
      , [ ("0x1234567890123456789012345678901234567893", 2, 1)
        , ("0x1234567890123456789012345678901234567894", 3, 1)
        ]
      )
    , ( [(0, True), (1, False), (3, True)]
      , [("0x1234567890123456789012345678901234567895", 4, 1)]
      )
    , ( [(0, True), (1, False), (3, True), (4, False)]
      , [("0x1234567890123456789012345678901234567893", 2, 1)]
      )
    , ( [(0, True), (1, False), (3, True), (4, True)]
      , [("0x1234567890123456789012345678901234567894", 3, 1)]
      )
    , ( [(0, True), (1, True)]
      , [ ("0x1234567890123456789012345678901234567893", 2, 1)
        , ("0x1234567890123456789012345678901234567894", 3, 1)
        , ("0x1234567890123456789012345678901234567895", 4, 1)
        ]
      )
    , ( [(0, True), (1, True), (2, False)]
      , [("0x1234567890123456789012345678901234567891", 0, 1)]
      )
    , ( [(0, True), (1, True), (2, True)]
      , [("0x1234567890123456789012345678901234567892", 1, 1)]
      )
    ]

-- Test that the activateMap function returns a correct map given a function
activateMapSimple :: Spec
activateMapSimple = do
  it "activateMapSimple" $ do
    activateMap (intermediateCompile (parse' src)) `shouldBe` activateMap'

 where
  src
    = "both( if true within seconds(1) then scale(1, 1, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(7, 7, transfer(0xdddddddddddddddddddddddddddddddddddddddd,0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee,0xffffffffffffffffffffffffffffffffffffffff)),  if true within seconds(2) then scale(17, 17, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else if true within seconds(3) then scale(53, 53, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(101, 101, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) )"
  activateMap' =
    (Map.fromList
      [ (("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0), 102)
      , (("0xdddddddddddddddddddddddddddddddddddddddd", 2), 7)
      ]
    )

marginRefundMapSimple :: Spec
marginRefundMapSimple = do
  it "marginRefundMapSimple" $ do
    marginRefundMap (intermediateCompile (parse' src)) `shouldBe` refundMap

 where
  src
    = "both( if true within seconds(1) then scale(1, 1, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(7, 7, transfer(0xdddddddddddddddddddddddddddddddddddddddd,0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee,0xffffffffffffffffffffffffffffffffffffffff)),  if true within seconds(2) then scale(17, 17, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else if true within seconds(3) then scale(53, 53, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(101, 101, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) )"
  refundMap =
    (Map.fromList
      [ ([(0, False)], [("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0, 1)])
      , ([(0, True)] , [("0xdddddddddddddddddddddddddddddddddddddddd", 2, 7)])
      , ( [(1, False), (2, True)]
        , [("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0, 48)]
        )
      , ([(1, True)], [("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0, 84)])
      ]
    )

-- Test that the activateMap works for the canonical IW source code
activateMapCanonicalIw :: Spec
activateMapCanonicalIw = do
  it "activateMapCanonicalIw" $ do
    activateMap (intermediateCompile (parse' canonical_iw_source))
      `shouldBe` activateMap'

 where
  activateMap' =
    (Map.fromList
      [ (("0x1234567890123456789012345678901234567891", 0), 1)
      , (("0x1234567890123456789012345678901234567892", 1), 1)
      , (("0x1234567890123456789012345678901234567893", 2), 1)
      , (("0x1234567890123456789012345678901234567894", 3), 1)
      , (("0x1234567890123456789012345678901234567895", 4), 1)
      , (("0x1234567890123456789012345678901234567896", 5), 1)
      , (("0x1234567890123456789012345678901234567897", 6), 1)
      , (("0x1234567890123456789012345678901234567898", 7), 1)
      ]
    )

refundMapCanonicalIw :: Spec
refundMapCanonicalIw = do
  it "refundMapCanonicalIw" $ do
    marginRefundMap (intermediateCompile (parse' canonical_iw_source))
      `shouldBe` activateMap

 where
  activateMap =
    (Map.fromList
      [ ( [(0, False)]
        , [ ("0x1234567890123456789012345678901234567891", 0, 1)
          , ("0x1234567890123456789012345678901234567892", 1, 1)
          , ("0x1234567890123456789012345678901234567893", 2, 1)
          , ("0x1234567890123456789012345678901234567894", 3, 1)
          , ("0x1234567890123456789012345678901234567895", 4, 1)
          ]
        )
      , ( [(0, False), (5, False)]
        , [("0x1234567890123456789012345678901234567896", 5, 1)]
        )
      , ( [(0, False), (5, False), (6, False)]
        , [("0x1234567890123456789012345678901234567897", 6, 1)]
        )
      , ( [(0, False), (5, False), (6, True)]
        , [("0x1234567890123456789012345678901234567898", 7, 1)]
        )
      , ( [(0, False), (5, True)]
        , [ ("0x1234567890123456789012345678901234567897", 6, 1)
          , ("0x1234567890123456789012345678901234567898", 7, 1)
          ]
        )
      , ( [(0, True)]
        , [ ("0x1234567890123456789012345678901234567896", 5, 1)
          , ("0x1234567890123456789012345678901234567897", 6, 1)
          , ("0x1234567890123456789012345678901234567898", 7, 1)
          ]
        )
      , ( [(0, True), (1, False)]
        , [ ("0x1234567890123456789012345678901234567891", 0, 1)
          , ("0x1234567890123456789012345678901234567892", 1, 1)
          ]
        )
      , ( [(0, True), (1, False), (3, False)]
        , [ ("0x1234567890123456789012345678901234567893", 2, 1)
          , ("0x1234567890123456789012345678901234567894", 3, 1)
          ]
        )
      , ( [(0, True), (1, False), (3, True)]
        , [("0x1234567890123456789012345678901234567895", 4, 1)]
        )
      , ( [(0, True), (1, False), (3, True), (4, False)]
        , [("0x1234567890123456789012345678901234567893", 2, 1)]
        )
      , ( [(0, True), (1, False), (3, True), (4, True)]
        , [("0x1234567890123456789012345678901234567894", 3, 1)]
        )
      , ( [(0, True), (1, True)]
        , [ ("0x1234567890123456789012345678901234567893", 2, 1)
          , ("0x1234567890123456789012345678901234567894", 3, 1)
          , ("0x1234567890123456789012345678901234567895", 4, 1)
          ]
        )
      , ( [(0, True), (1, True), (2, False)]
        , [("0x1234567890123456789012345678901234567891", 0, 1)]
        )
      , ( [(0, True), (1, True), (2, True)]
        , [("0x1234567890123456789012345678901234567892", 1, 1)]
        )
      ]
    )

timeTranslationIMemExpTest :: Spec
timeTranslationIMemExpTest = do
  it "translates time properly" $ do
    intermediateCompile contract `shouldBe` intermediateContract

 where
  contract :: Contract
  contract =
    makeContract defaultAddressMap
      $  "translate(minutes(2), if (obs(bool, O, 0)) within minutes(2) "
      ++ "then transfer(T, A, B) "
      ++ "else scale(2, 2, transfer(T, A, B)))"

  intermediateContract :: IntermediateContract
  intermediateContract =
    IntermediateContract parties transfers memExps activateMap marginRefundMap
  parties = [Bound oneAddr, Bound twoAddr]

  transfers =
    [ TransferCall { maxAmount    = 1
                   , amount       = Lit (IntVal 1)
                   , delay        = 120
                   , tokenAddress = tokAddr
                   , from         = 0
                   , to           = 1
                   , memExpPath   = [(0, True)]
                   }
    , TransferCall { maxAmount    = 2
                   , amount       = MultExp (Lit (IntVal 1)) (Lit (IntVal 2))
                   , delay        = 120
                   , tokenAddress = tokAddr
                   , from         = 0
                   , to           = 1
                   , memExpPath   = [(0, False)]
                   }
    ]

  memExps         = [IMemExp 120 240 0 (Lit (Observable OBool obsAddr "0"))]

  activateMap     = Map.fromList [((tokAddr, 0), 2)]

  marginRefundMap = Map.fromList [([(0, True)], [(tokAddr, 0, 1)])]

zeroContractCodeTest :: Spec
zeroContractCodeTest = do
  it "translates zero contracts into no TCs" $ do
    intermediateCompile Zero `shouldBe` emptyContract

  it "translates an if-within that contains a zero contract" $ do
    intermediateCompile contract `shouldBe` intermediateContract

 where
  contract :: Contract
  contract =
    makeContract defaultAddressMap
      $  "if obs(bool, O, 0) within seconds(10) "
      ++ "then transfer(T, A, B) else zero"

  intermediateContract :: IntermediateContract
  intermediateContract =
    IntermediateContract parties transfers memExps activateMap marginRefundMap

  parties = [Bound oneAddr, Bound twoAddr]

  transfers =
    [ TransferCall { maxAmount    = 1
                   , amount       = Lit (IntVal 1)
                   , delay        = 0
                   , tokenAddress = tokAddr
                   , from         = 0
                   , to           = 1
                   , memExpPath   = [(0, True)]
                   }
    ]

  memExps =
    [ IMemExp { _IMemExpBegin = 0
              , _IMemExpEnd   = 10
              , _IMemExpIdent = 0
              , _IMemExp      = Lit (Observable OBool obsAddr "0")
              }
    ]

  activateMap     = Map.fromList [((tokAddr, 0), 1)]

  marginRefundMap = Map.fromList [([(0, False)], [(tokAddr, 0, 1)])]

basicTransferTest :: Spec
basicTransferTest = do
  it "compiles a basic transfer" $ do
    intermediateCompile transfer `shouldBe` transferIC

 where
  transfer :: Contract
  transfer = Transfer { tokenAddress_ = tokAddr
                      , to_           = Bound oneAddr
                      , from_         = Bound twoAddr
                      }

  transferIC :: IntermediateContract
  transferIC = IntermediateContract
    [Bound twoAddr, Bound oneAddr]

    [ TransferCall { maxAmount    = 1
                   , amount       = Lit (IntVal 1)
                   , delay        = 0
                   , tokenAddress = tokAddr
                   , to           = 1
                   , from         = 0
                   , memExpPath   = []
                   }
    ]

    []
    (Map.fromList [((tokAddr, 0), 1)])
    Map.empty
