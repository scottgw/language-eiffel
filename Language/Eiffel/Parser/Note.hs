module Language.Eiffel.Parser.Note where

import Control.Applicative hiding ((<|>), optional)

import Language.Eiffel.Note
import Language.Eiffel.Parser.Lex

import Text.Parsec

note :: Parser [Note]
note = keyword "note" >> many1 noteItem

noteItem :: Parser Note
noteItem = Note <$> (identifier <* colon)
                <*> strOrListIdent
                <* optional semicolon

strOrListIdent :: Parser (Either String [String])
strOrListIdent = 
  (Left <$> anyStringTok) <|> (Right <$> identifier `sepBy1` comma)
