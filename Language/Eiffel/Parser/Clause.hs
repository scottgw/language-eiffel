module Language.Eiffel.Parser.Clause where

import Control.Applicative ((<$>), (<*))

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Expr
import Language.Eiffel.Parser.Lex

import Text.Parsec

clause :: Parser (Clause Expr)
clause = do 
  tag <- try (Just <$> identifier <* colon) <|> return Nothing
  Clause tag <$> expr
