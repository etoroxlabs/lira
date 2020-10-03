{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

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

module Lira.Contract.Pretty
  ( printContract
  ) where

import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Lira.Contract

printContract :: Contract -> Text
printContract = renderStrict . layoutPretty defaultLayoutOptions . ppC

ppC :: Contract -> Doc ann
ppC (Transfer tok from to) = ppFun "transfer" [pretty (pack tok), ppParty from, ppParty to]
ppC (Scale maxf scalef con) = ppFun "scale" [pretty maxf, ppExpr scalef, ppC con]
ppC (Both con1 con2) = ppFun "both" [ppC con1, ppC con2]
ppC (Translate delay con) = ppFun "translate" [ppT delay, ppC con]
ppC (IfWithin memExp con1 con2) = ppIfWithin memExp con1 con2
ppC Zero = pretty' "zero"

ppParty :: Party -> Doc ann
ppParty = \case
  Bound addr -> pretty addr
  Free ident -> pretty' "free" <> tupled [pretty ident]

ppFun :: Text -> [Doc ann] -> Doc ann
ppFun combinator args = pretty combinator <> tupled args

ppT :: Time -> Doc ann
ppT = \case
  Now -> pretty' "now"
  Seconds i -> ppT' "seconds" i
  Minutes i -> ppT' "minutes" i
  Hours i -> ppT' "hours" i
  Days i -> ppT' "days" i
  Weeks i -> ppT' "weeks" i

ppT' :: Text -> Integer -> Doc ann
ppT' time duration =
  pretty time <> tupled [pretty duration]

ppIfWithin :: MemExp -> Contract -> Contract -> Doc ann
ppIfWithin (MemExp time expr) con1 con2 =
  pretty' "if" <+> ppExpr expr <+> pretty' "within" <+> ppT time
    <+> pretty' "then" <+> ppC con1
    <+> pretty' "else" <+> ppC con2

tshow :: Show a => a -> Text
tshow = pack . show

prec :: Expr -> Int
prec e = case e of
  Lit _         -> 0
  MinExp    _ _ -> 0
  MaxExp    _ _ -> 0

  MultExp   _ _ -> 1
  DiviExp   _ _ -> 1
  AddiExp   _ _ -> 2
  SubtExp   _ _ -> 2

  LtExp     _ _ -> 3
  GtExp     _ _ -> 3
  EqExp     _ _ -> 3 -- ?
  GtOrEqExp _ _ -> 3
  LtOrEqExp _ _ -> 3
  NotExp _      -> 4
  AndExp _ _    -> 5
  OrExp  _ _    -> 6
  IfExp _ _ _   -> 7

isAssoc :: Expr -> Bool
isAssoc e = case e of
  MultExp _ _ -> True
  AddiExp _ _ -> True
  _           -> False

ppBinOp :: Expr -> Expr -> Expr -> Doc ann
ppBinOp parentE leftE rightE =
  ppBinOp' leftE True <+> pretty op <+> ppBinOp' rightE False
  where
    ppBinOp' :: Expr -> Bool -> Doc ann
    ppBinOp' e isLeft
      | prec e < prec parentE = ppExpr e
      | prec e == prec parentE && isLeftAssociative e isLeft = ppExpr e
      | otherwise = tupled [ppExpr e]

    isLeftAssociative :: Expr -> Bool -> Bool
    isLeftAssociative e isLeft =
      isAssoc e && isAssoc parentE && isLeft

    op :: Text
    op = case parentE of
      MultExp   _ _ -> "*"
      DiviExp   _ _ -> "/"
      AddiExp   _ _ -> "+"
      SubtExp   _ _ -> "-"
      LtExp     _ _ -> "<"
      GtExp     _ _ -> ">"
      EqExp     _ _ -> "="
      GtOrEqExp _ _ -> ">="
      LtOrEqExp _ _ -> "<="
      AndExp    _ _ -> "and"
      OrExp     _ _ -> "or"

ppExpr :: Expr -> Doc ann
ppExpr e = case e of
  Lit (IntVal i) -> pretty i
  Lit (BoolVal True) -> pretty' "true"
  Lit (BoolVal False) -> pretty' "false"
  Lit (Observable obsType addr key) -> pretty' "obs" <> tupled [ ppObsType obsType, pretty addr, pretty key ]

  MinExp    e1 e2 -> ppFun "min" [ppExpr e1, ppExpr e2]
  MaxExp    e1 e2 -> ppFun "max" [ppExpr e1, ppExpr e2]

  MultExp   e1 e2 -> ppBinOp e e1 e2
  DiviExp   e1 e2 -> ppBinOp e e1 e2
  AddiExp   e1 e2 -> ppBinOp e e1 e2
  SubtExp   e1 e2 -> ppBinOp e e1 e2
  LtExp     e1 e2 -> ppBinOp e e1 e2
  GtExp     e1 e2 -> ppBinOp e e1 e2
  EqExp     e1 e2 -> ppBinOp e e1 e2
  GtOrEqExp e1 e2 -> ppBinOp e e1 e2
  LtOrEqExp e1 e2 -> ppBinOp e e1 e2
  AndExp    e1 e2 -> ppBinOp e e1 e2
  OrExp     e1 e2 -> ppBinOp e e1 e2

  NotExp e1       -> ppFun "not" [ppExpr e1]
  IfExp e1 e2 e3  -> pretty' "if" <+> tupled [ppExpr e1] <+>
                     pretty' "then" <+> ppExpr e2 <+>
                     pretty' "else" <+> ppExpr e3

ppObsType :: ObservableType -> Doc ann
ppObsType = \case
  OBool -> pretty' "bool"
  OInteger -> pretty' "int"

pretty' :: Text -> Doc ann
pretty' = pretty @Text
