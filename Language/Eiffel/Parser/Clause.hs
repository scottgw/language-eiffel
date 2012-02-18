module Language.Eiffel.Parser.Clause where

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Expr
import Language.Eiffel.Parser.Lex

clause :: Parser (Clause Expr)
clause = do 
  tag <- identifier
  colon
  Clause tag `fmap` expr
