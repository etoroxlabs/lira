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

module Lira.Contract.Intermediate where

import           Lira.Contract

import qualified Data.Map.Strict               as Map

type PartyIndex = Integer
type PartyIdentifier = Integer

type MemExpId = Integer
type Branch = Bool
type MemExpPath = [(MemExpId, Branch)]

data IntermediateContract = IntermediateContract
  { parties         :: [Party]
  , transferCalls   :: [TransferCall]
  , memExps         :: [IMemExp]
  , activateMap     :: ActivateMap
  , marginRefundMap :: MarginRefundMap
  } deriving (Show, Eq)

data TransferCall = TransferCall
  { maxAmount    :: Integer
  , amount       :: Expr
  , delay        :: Integer
  , tokenAddress :: Address
  , from         :: PartyIndex
  , to           :: PartyIndex
  , memExpPath   :: MemExpPath
  } deriving (Show, Eq)

-- DEVNOTE:
-- We start by attempting to implement the evaluation of IMemExp values.
-- Later we try to find out how to read them when tcalls are executed.
data IMemExp = IMemExp
  { _IMemExpBegin  :: Integer
  , _IMemExpEnd    :: Integer
  , _IMemExpIdent  :: Integer
  , _IMemExp       :: Expr
  } deriving (Show, Eq)

type ActivateMap = Map.Map (Address, PartyIndex) Integer

type MarginRefundMap
  = Map.Map [(Integer, Bool)] [(Address, PartyIndex, Integer)]

-- (path, marginRefundValue) = ([(memExpRef, branch (true or false))], (token address, recipient, amount))
type MarginRefundMapElement
  = ([(Integer, Bool)], [(Address, PartyIndex, Integer)])

type MarginRefundPath = [(Integer, Bool)]

-- (token address, from address, amount)
type ActivateMapElement = ((Address, PartyIndex), Integer)
