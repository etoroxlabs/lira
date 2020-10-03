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

module Lira.Contract.Parser
  ( parseContract
  , parseContract'
  ) where

import           Control.Monad (void)
import           Data.Bifunctor (first)
import           Data.Char (isDigit, isLetter)
import           Data.Functor (($>), (<$))
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char -- (space, digitChar, hexDigitChar, char, char')
import           Control.Monad.Combinators.Expr

import           Lira.Contract

type Parser = Parsec Void Text

parseContract :: FilePath -> Text -> Either Text Contract
parseContract srcFile srcText =
  first (Text.pack . show) $ parseContract' srcFile srcText

parseContract'
  :: FilePath
  -> Text
  -> Either (ParseErrorBundle Text Void) Contract
parseContract' srcFile srcText =
  parse (space *> contractP <* eof) srcFile srcText

contractP :: Parser Contract
contractP = choice
  [ comb "transfer" $ Transfer <$> (addressP <* symbol ",")
                               <*> (partyP <* symbol ",")
                               <*> partyP
  , comb "scale" $ Scale <$> (integerP <* symbol ",")
                         <*> (exprP <* symbol ",")
                         <*> contractP
  , comb "both" $ Both <$> (contractP <* symbol ",")
                       <*> contractP
  , comb "translate" $ Translate <$> (timeP <* symbol ",")
                                 <*> contractP
  , ifWithinP
  , symbol "zero" $> Zero
  ]

ifWithinP :: Parser Contract
ifWithinP =
  IfWithin <$> (symbol "if" *> memExpP)
           <*> (symbol "then" *> contractP)
           <*> (symbol "else" *> contractP)
  where
    memExpP = do
      cond <- exprP
      time <- symbol "within" *> timeP
      pure (MemExp time cond)

timeP :: Parser Time
timeP = choice
  [ symbol "now" $> Now
  , timeP' "seconds" Seconds
  , timeP' "minutes" Minutes
  , timeP' "hours" Hours
  , timeP' "days" Days
  , timeP' "weeks" Weeks
  ]
  where
    timeP' :: Text -> (Integer -> Time) -> Parser Time
    timeP' s timeF = labelText s $ symbol s *> fmap timeF (parens integerP)

exprP :: Parser Expr
exprP = ifExprP <|> makeExprParser termP
  [ [ InfixL (MultExp <$ symbol "*")
    , InfixL (DiviExp <$ symbol "/")
    ]
  , [ InfixL (AddiExp <$ symbol "+")
    , InfixL (SubtExp <$ symbol "-")
    ]
  , [ InfixN (EqExp <$ symbol "=")
    , InfixN (GtOrEqExp <$ symbol ">=")
    , InfixN (LtOrEqExp <$ symbol "<=")
    , InfixN (LtExp <$ symbol "<")
    , InfixN (GtExp <$ symbol ">")
    ]
  , [ InfixL (AndExp <$ keyword "and") ]
  , [ InfixL (OrExp <$ keyword "or") ]
  ]

-- FIXME: Handles 0 whitespace between 'if' and arg. Use 'keyword' approach from hs-jq.
ifExprP :: Parser Expr
ifExprP =
  IfExp <$> (symbol "if" *> parens exprP)
        <*> (symbol "then" *> exprP)
        <*> (symbol "else" *> exprP)

termP :: Parser Expr
termP = choice
  [ comb "min" $ MinExp <$> (exprP <* symbol ",") <*> exprP
  , comb "max" $ MaxExp <$> (exprP <* symbol ",") <*> exprP
  , keyword "true" $> Lit (BoolVal True)
  , keyword "false" $> Lit (BoolVal False)
  , (keyword "not" $> NotExp) <*> parens exprP
  , Lit . IntVal <$> integerP
  , comb "obs" $ fmap Lit obsP
  , parens exprP
  ]

addressP :: Parser Address
addressP = lexeme $ do
  chunk "0x"
  addr <- count 40 hexDigitChar
  pure ("0x" ++ addr)

partyP :: Parser Party
partyP = choice
  [ Bound <$> addressP
  , Free <$> (symbol "free" >> parens integerP)
  ]

integerP :: Parser Integer
integerP = lexeme . fmap readText $
  takeWhile1P (Just "non-negative integer") isDigit

obsP :: Parser Literal
obsP = Observable <$> (obsTypeP <* symbol ",")
                  <*> (addressP <* symbol ",")
                  <*> obsKeyP

obsTypeP :: Parser ObservableType
obsTypeP = choice
  [ OBool <$ symbol "bool"
  , OInteger <$ symbol "integer"
  ]

obsKeyP :: Parser String
obsKeyP = some hexDigitChar -- FIXME: Why hex digits?

comb :: Text -> Parser a -> Parser a
comb s p = labelText s $ symbol s *> parens p

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

symbol :: Text -> Parser ()
symbol = lexeme . void . chunk

keyword :: Text -> Parser ()
keyword s = lexeme $ chunk s >> notFollowedBy (satisfy isLetter)

lexeme :: Parser a -> Parser a
lexeme = (<* space)

readText :: Read a => Text -> a
readText = read . Text.unpack

labelText :: Text -> Parser a -> Parser a
labelText t p = label (Text.unpack t) p
