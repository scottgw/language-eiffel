module Language.Eiffel.Parser.Clause where

import Control.Applicative ((<$>), (<*))

import Language.Eiffel.Syntax

import Language.Eiffel.Parser.Expr
import Language.Eiffel.Parser.Lex
import Language.Eiffel.Position

import Text.Parsec

clause :: Parser (Clause Expr)
clause = do
  let tagNoExpr = do
        i <- identifier <* colon
        p <- getPosition
        e <- option (attachPos p (LitBool True)) expr
        optional semicolon
        return (Clause (Just i) e)
      withExpr = do
        tag <- try (Just <$> identifier <* colon) <|> return Nothing
        e <- expr
        optional semicolon
        return (Clause tag e)
    in try tagNoExpr <|> withExpr
