module Language.Eiffel.Parser.Clause where

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Expr
import Language.Eiffel.Parser.Lex

import Text.Parsec

clause :: Parser (Clause Expr)
clause = do 
  tag <- try (do tag <- identifier
                 colon
                 return (Just tag)) <|> return Nothing
  Clause tag `fmap` expr
