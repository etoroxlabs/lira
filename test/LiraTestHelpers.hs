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

module LiraTestHelpers
  ( makeContract
  , defaultAddressMap
  , obsAddr
  , tokAddr
  , oneAddr
  , twoAddr
  , parse' -- FIXME: Deprecate this.
  )
where

import           Data.Either (fromRight)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Lira.Contract
import           Lira.Contract.Parser (parseContract)

obsAddr, tokAddr, oneAddr, twoAddr :: Address
obsAddr = "0x1111111111111111111111111111111111111111"
tokAddr = "0x2222222222222222222222222222222222222222"
oneAddr = "0x3333333333333333333333333333333333333333"
twoAddr = "0x4444444444444444444444444444444444444444"

defaultAddressMap :: Map Char Address
defaultAddressMap =
  Map.fromList [('O', obsAddr), ('T', tokAddr), ('A', oneAddr), ('B', twoAddr)]

-- FIXME: These wrappers were from before deprecating Lira.Parser in favor
-- of Lira.Contract.Parser. This parser features better error handling and
-- was made with Megaparsec. The tests need cleaning up and a starting point
-- is the removal of makeContract and parse' below:

makeContract :: Map Char Address -> String -> Contract
makeContract addressMap = parse' . insertAddr
  where
    insertAddr srcCode =
      srcCode >>= \c -> Map.findWithDefault [c] c addressMap

parse' :: String -> Contract
parse' s = case parseContract "error" (Text.pack s) of
  Left  err -> error (Text.unpack err)
  Right ast -> ast
