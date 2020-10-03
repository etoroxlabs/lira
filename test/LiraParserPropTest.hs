
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

module LiraParserPropTest
  ( tests
  , hprop_ParsePrettyPrintIdentity
  )
where

import qualified Data.Text as Text

import           Data.Text (unpack)

import           Lira.Contract
import           Lira.Contract.Pretty (printContract)
import           Lira.Contract.Parser (parseContract')
import           LiraGen

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog (PropertyT, forAll, hedgehog, (===))


tests :: Spec
tests =
  describe "parseContract/printContract" $
    it "are inverses" $
      hedgehog hprop_ParsePrettyPrintIdentity

hprop_ParsePrettyPrintIdentity :: PropertyT IO ()
hprop_ParsePrettyPrintIdentity = do
  c <- forAll contractGen
  let s = printContract c
  got <- evalEither (parseContract' "" s)
  c === got
