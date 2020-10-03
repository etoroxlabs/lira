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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lira.Backends.Evm.Abi where

import           Data.Aeson
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics

data AbiVarDefinition = AbiVarDefinition {
    name_ :: String
  , type_ :: String
  } deriving (Show)

instance ToJSON AbiVarDefinition where
  toJSON (AbiVarDefinition n t) = object ["name" .= n, "type" .= t]

data AbiConstructorDefinition = AbiConstructorDefinition {
    payable__ :: Bool
  , type__    :: String
  , inputs__  :: [AbiVarDefinition]
  } deriving (Show)

instance ToJSON AbiConstructorDefinition where
  toJSON (AbiConstructorDefinition p t is) =
    object ["payable" .= p, "type" .= t, "inputs" .= toJSON is]

data AbiFunctionDefinition = AbiFunctionDefinition
  { _name     :: String
  , _type     :: String
  , _payable  :: Bool
  , _outputs  :: [AbiVarDefinition]
  , _inputs   :: [AbiVarDefinition]
  , _constant :: Bool
  } deriving (Generic, Show)

instance ToJSON AbiFunctionDefinition where
  toJSON (AbiFunctionDefinition n t p os is c) = object
    [ "name" .= n
    , "type" .= t
    , "payable" .= p
    , "inputs" .= toJSON is
    , "outputs" .= toJSON os
    , "constant" .= c
    ]

data AbiEventDefinition = AbiEventDefinition
  { _eventName      :: String
  , _eventType      :: String
  , _eventAnonymous :: Bool
  , _eventInputs    :: [AbiVarDefinition]
  } deriving (Generic, Show)

instance ToJSON AbiEventDefinition where
  toJSON (AbiEventDefinition n t a i) =
    object ["name" .= n, "type" .= t, "inputs" .= i, "anonymous" .= a]

data AbiDefinition = AbiDefinition {
    constuctor :: Maybe AbiConstructorDefinition
  , functions  :: [AbiFunctionDefinition]
  , events     :: [AbiEventDefinition]
  } deriving (Show)

-- The JSON type of this should be [abiConstructor, abiFucntions]
instance ToJSON AbiDefinition where
  toJSON (AbiDefinition constructor functions events) = case constructor of
    Nothing -> toJSONList functions
    -- DEVFIX: THIS NEEDS TO HAVE THE CONSTRUCTOR ADDED!!!
    --Just c  -> toJSON $ ( [(toJSON c), (toJSONList functions)])
    Just c  -> toJSON $ map toJSON functions ++ map toJSON events

-- What kind of type should this take as argument?
-- DEVQ: Perhaps this should be calculated in EvmCompile?
abiDefinition :: AbiDefinition
abiDefinition =
  AbiDefinition constructor [execute, activate, take] [activatedE]
  where
    constructor = Just (AbiConstructorDefinition False "constructor" [])
    execute = AbiFunctionDefinition "execute" "function" False [] [] False
    activate = AbiFunctionDefinition "activate" "function" False [] [] False
    take = AbiFunctionDefinition
             "take" "function" False []
             [AbiVarDefinition "party" "uint256"] False
    activatedE = AbiEventDefinition "Activated" "event" False []

encodeUtf8 :: ToJSON a => a -> Text
encodeUtf8 = decodeUtf8 . toStrict . encode
