module Language.Eiffel.Parser.Note where

import Control.Applicative hiding ((<|>))

import Language.Eiffel.Note
import Language.Eiffel.Parser.Lex

import Text.Parsec

note :: Parser [Note]
note = keyword "note" >> many1 noteItem

noteItem :: Parser Note
noteItem = Note <$> (identifier <* opNamed ":")
                <*> strOrListIdent

strOrListIdent :: Parser (Either String [String])
strOrListIdent = 
  (Left <$> stringTok) <|> (Right <$> identifier `sepBy1` comma)
