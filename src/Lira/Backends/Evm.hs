--- MIT License
-- 
-- Copyright (c) 2020 eToroX Labs
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

module Lira.Backends.Evm where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Lira.Backends.Evm.EvmCompiler (assemble)
import           Lira.Backends.Evm.Abi (encodeUtf8, abiDefinition)
import           Lira.Backends.IntermediateCompiler (intermediateCompile)
import           Lira.Contract.Parser (parseContract)
import           Lira.TypeChecker (typeCheck)

compile :: FilePath -> Text -> Either Text [(FilePath, Text)]
compile srcFile srcText = do
  contract <- parseContract srcFile srcText
  typeCheck contract
  pure [ ("file.abi", encodeUtf8 abiDefinition)
       , ("file.bin", assemble (intermediateCompile contract))
       ]
