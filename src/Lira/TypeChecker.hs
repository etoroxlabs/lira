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

{-# LANGUAGE OverloadedStrings #-}

module Lira.TypeChecker where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad (when)

import           Lira.Contract

data ExpType
  = BoolType
  | IntType deriving (Eq)

instance Show ExpType where
  show BoolType = "bool"
  show IntType = "int"

good :: Applicative m => m ()
good = pure ()

tshow :: Show a => a -> Text
tshow = Text.pack . show

typeCheck :: Contract -> Either Text ()
typeCheck Zero = good
typeCheck (Transfer token from to) = good
typeCheck (Both contract1 contract2) = do
  typeCheck contract1
  typeCheck contract2
typeCheck (Translate delay contract) = typeCheck contract
typeCheck (IfWithin (MemExp time e) contractThen contractElse) = do
  require e BoolType ("1st argument to if-within")
  typeCheck contractThen
  typeCheck contractElse
typeCheck (Scale maxFactor scaleFactor contract) = do
  require scaleFactor IntType ("2nd argument to scale")
  typeCheck contract

require :: Expr -> ExpType -> Text -> Either Text ExpType
require e t1 errmsg = do
  t2 <- getType e
  if t1 /= t2
    then Left (errmsg <> ", expected " <> tshow t1 <> ", got " <> tshow t2)
    else Right t1

require2 :: ExpType -> Expr -> Expr -> Text -> Either Text ExpType
require2 t0 e1 e2 errmsg = do
  t1 <- getType e1
  t2 <- getType e2
  if t0 /= t1 || t0 /= t2
    then Left (errmsg <> " expression: expected "
               <> tshow t0 <> "s, but got: "
               <> tshow t1 <> ", " <> tshow t2)
    else pure t0

require2' :: Expr -> Expr -> Text -> Either Text ExpType
require2' e1 e2 errmsg = do
  t1 <- getType e1
  t2 <- getType e2
  if t1 /= t2
    then Left (errmsg <> " expression must have same operand types, but got "
               <> tshow t1 <> ", " <> tshow t2)
    else pure BoolType

getType :: Expr -> Either Text ExpType
getType (Lit lit) = getLitType lit
getType (MinExp e1 e2) = require2 IntType e1 e2 "min"
getType (MaxExp e1 e2) = require2 IntType e1 e2 "max"
getType (MultExp e1 e2) = require2 IntType e1 e2 "*"
getType (DiviExp e1 e2) = require2 IntType e1 e2 "/"
getType (AddiExp e1 e2) = require2 IntType e1 e2 "+"
getType (SubtExp e1 e2) = require2 IntType e1 e2 "-"

getType (LtExp e1 e2) = require2 IntType e1 e2 "<" >> pure BoolType
getType (GtExp e1 e2) = require2 IntType e1 e2 ">" >> pure BoolType
getType (GtOrEqExp e1 e2) = require2 IntType e1 e2 ">=" >> pure BoolType
getType (LtOrEqExp e1 e2) = require2 IntType e1 e2 "<=" >> pure BoolType
getType (AndExp e1 e2) = require2 IntType e1 e2 "and" >> pure BoolType
getType (OrExp e1 e2) = require2 IntType e1 e2 "or" >> pure BoolType

getType (NotExp e1) = require e1 IntType "not"
getType (EqExp e1 e2) = require2' e1 e2 "="
getType (IfExp e1 e2 e3) = do
  require e1 BoolType "if condition must have bool condition"
  require2' e1 e2 "if-within"

getLitType :: Literal -> Either Text ExpType
getLitType (IntVal  _              ) = Right IntType
getLitType (BoolVal _              ) = Right BoolType
getLitType (Observable OBool    _ _) = Right BoolType
getLitType (Observable OInteger _ _) = Right IntType


{-
getType :: Expr -> Either Text ExpType
getType (Lit literal  ) = getLiteral literal
getType (MultExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return IntType
    else
      Left
      $  "Error in multiplication expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (SubtExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return IntType
    else
      Left
      $  "Error in subtraction expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (AddiExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return IntType
    else
      Left
      $  "Error in addition expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (DiviExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return IntType
    else
      Left
      $  "Error in division expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (LtExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return BoolType
    else
      Left
      $  "Error in LtExp expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (GtExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return BoolType
    else
      Left
      $  "Error in GtExp expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (EqExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return BoolType
    else
      Left
      $  "Error in EqExp expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (GtOrEqExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return BoolType
    else
      Left
      $  "Error in GtOrEqExp expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (LtOrEqExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return BoolType
    else
      Left
      $  "Error in LtOrEqExp expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (OrExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == BoolType && t1 == BoolType
    then return BoolType
    else
      Left
      $  "Error in OrExp expression! Expected bool, bool; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (AndExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == BoolType && t1 == BoolType
    then return BoolType
    else
      Left
      $  "Error in AndExp expression! Expected bool, bool; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (MinExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return IntType
    else
      Left
      $  "Error in MinExp expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (MaxExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType
    then return IntType
    else
      Left
      $  "Error in MaxExp expression! Expected int, int; got "
      <> tshow t0
      <> ", "
      <> tshow t1
getType (NotExp e0) = do
  t0 <- getType e0
  if t0 == BoolType
    then return BoolType
    else Left $ "Error in NotExp expression! Expected bool; got " <> tshow t0
getType (IfExp e0 e1 e2) = do
  t0 <- getType e0
  t1 <- getType e1
  t2 <- getType e2
  if t0
     == BoolType
     && ((t1 == BoolType && t2 == BoolType) || (t1 == IntType && t2 == IntType))
  then
    return $ if t1 == BoolType then BoolType else IntType
  else
    if t0 /= BoolType
      then
        Left
        $  "Error in IfExp expression! First exp must be of type bool; got "
        <> tshow t0
      else
        Left
        $  "Error in IfExp expression! Types in both branches must match; got "
        <> tshow t1
        <> ", "
        <> tshow t2
-}

