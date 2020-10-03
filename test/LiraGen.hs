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

module LiraGen where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Lira.Contract

-- Generate contracts that type-check

positiveGen = Gen.integral (Range.constantFrom 0 0 10000)

contractGen :: Gen Contract
contractGen =
  Gen.recursive Gen.choice
    [ zeroGen, transferGen ]
    [ scaleGen, bothGen, translateGen, ifWithinGen ]
  where
    zeroGen = pure Zero
    transferGen = Transfer <$> addressGen <*> partyGen <*> partyGen
    scaleGen = Scale <$> positiveGen <*> intExprGen <*> contractGen
    bothGen = Both <$> contractGen <*> contractGen
    translateGen = Translate <$> timeGen <*> contractGen
    ifWithinGen = IfWithin <$> memExpGen <*> contractGen <*> contractGen

    addressGen = ("0x" ++) <$> Gen.list (Range.singleton 40) Gen.hexit
    memExpGen = MemExp <$> timeGen <*> boolExprGen
    partyGen = Gen.choice
      [ Free <$> Gen.integral (Range.constantFrom 0 0 255)
      , Bound <$> addressGen ]
    timeGen = Gen.choice $
      pure Now : map (<$> positiveGen) [ Seconds, Minutes, Hours, Days, Weeks ]

intExprGen :: Gen Expr
intExprGen =
  Gen.recursive Gen.choice
    [ Lit . IntVal <$> positiveGen ]
    ( intBinOps ++ [ ifExprGen intExprGen ] )
  where
    intBinOps = map (binOpGen intExprGen) [ MinExp, MaxExp, MultExp, DiviExp, AddiExp, SubtExp ]

boolExprGen :: Gen Expr
boolExprGen =
  Gen.recursive Gen.choice
    [ Lit . BoolVal <$> Gen.element [ True, False ], NotExp <$> boolExprGen ]
    ( intBinOps ++ boolBinOps ++ [ ifExprGen boolExprGen ] )
  where
    intBinOps = map (binOpGen intExprGen) [ LtExp, GtExp, EqExp, GtOrEqExp, LtOrEqExp ]
    boolBinOps = map (binOpGen boolExprGen) [ AndExp, OrExp ]

binOpGen :: Gen Expr -> (Expr -> Expr -> Expr) -> Gen Expr
binOpGen g op = op <$> g <*> g

ifExprGen :: Gen Expr -> Gen Expr
ifExprGen argGen =
  IfExp <$> boolExprGen <*> argGen <*> argGen
