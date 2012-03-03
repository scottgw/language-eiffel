module Language.Eiffel.Parser.Note where

import Control.Applicative hiding ((<|>), optional)

import Language.Eiffel.Note
import Language.Eiffel.Expr
import Language.Eiffel.Parser.Lex
import Language.Eiffel.Parser.Expr

import Text.Parsec

note :: Parser [Note]
note = keyword "note" >> many1 noteEntry

noteEntry :: Parser Note
noteEntry = Note <$> (identifier <* colon)
                <*> noteItem `sepBy1` comma
                <* optional semicolon

noteItem :: Parser UnPosExpr
noteItem =  VarOrCall <$> identifier <|>
            stringLit <|>
            charLit <|>
            intLit <|>
            boolLit <|>
            doubleLit
