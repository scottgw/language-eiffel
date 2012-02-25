{-# LANGUAGE FlexibleContexts #-}
module Language.Eiffel.Parser.Lex (Token (..),
                   SpanToken (..),
                   Parser,
                   tokenizer,
                   opSymbol,
                   attachTokenPos,
                   justToken,
                   opNamed,
                   someOp,
                   someKeyword,
                   identifier,
                   period,
                   anyStringTok,
                   stringTok,
                   blockStringTok,
                   charTok,
                   boolTok,
                   integerTok,
                   floatTok,
                   parens,
                   squares,
                   braces,
                   angles,
                   keyword,
                   comma,
                   colon,
                   semicolon
                  ) where

import Control.Applicative ((<$>))
import Control.Monad

import Text.Parsec hiding (token)
import qualified Text.Parsec as P (token)
import qualified Text.Parsec.ByteString as P
import qualified Text.Parsec.Token as P

import Language.Eiffel.Eiffel

type Parser a = Parsec [SpanToken] () a

data Token 
    = Identifier String
    | Keyword String
    | Symbol Char
    | String String
    | BlockString String
    | Char Char
    | Bool Bool
    | Paren Char
    | Operator String
    | Float Double
    | Integer Integer
      deriving Show

-- create paren/bracket token
-- symbol token for non-operator symbols, possibly combined with the above parens

data SpanToken = 
    SpanToken { spanToken :: Token,
                spanP :: SourcePos} deriving Show

attachTokenPos :: Parser a -> Parser (Pos a)
attachTokenPos p = do
  tks <- getInput
  case tks of
    t:_ -> attachPos (spanP t) <$> p
    _ -> fail "no more input"

justToken :: Parser Token
justToken = spanToken `fmap` anyToken


token :: P.Parser SpanToken
token
    = do p <- getPosition
         t <- token' <?> "Token"
         return (SpanToken t p)

token' :: P.Parser Token
token'
    = Bool <$> (bool <?> "Bool")
      <|> Identifier <$> (identifierL <?> "Identifier")
      <|> Operator <$> (operator <?> "Operator")      
      <|> Keyword <$> (keywordL <?> "Keyword")
      <|> Char <$> (charLex <?> "Char")    
      <|> Float <$> try (float <?> "Float")
      <|> Integer <$> (integer <?> "Integer")
      <|> Symbol <$> (symbolChar <?> "paren")
      <|> BlockString <$> (blockString <?> "Block string")      
      <|> String <$> (stringLiteral <?> "String lit")


comma :: Parser ()
comma = symbolNamed ','

period :: Parser ()
period = symbolNamed '.'

semicolon :: Parser ()
semicolon = symbolNamed ';'


colon = opNamed ":"


myToken f = P.token show spanP (f . spanToken) 

someOp :: Parser String
someOp = myToken anyOperator <?> "operator"

someKeyword :: Parser String
someKeyword = myToken anyKeyword <?> "keyword"

opNamed :: String -> Parser ()
opNamed name = myToken (matchOperator name) <?> ("'" ++ name ++ "'")

anyOperator (Operator op) = Just op
anyOperator _ = Nothing

matchOperator n (Operator op) | n == op = Just ()
                              | otherwise = Nothing
matchOperator _ _ = Nothing

anyKeyword (Keyword k) = Just k
anyKeyword _ = Nothing

matchKeyword n (Keyword k) | k == n = Just ()
                           | otherwise = Nothing
matchKeyword _ _ = Nothing



keyword :: String -> Parser ()
keyword name = myToken (matchKeyword name) <?> ("'" ++ name ++ "'")

anyIdentifier (Identifier i) = Just i
anyIdentifier _ = Nothing

identifier :: Parser String
identifier = myToken anyIdentifier

anyInteger (Integer i) = Just i
anyInteger _ = Nothing

integerTok :: Parser Integer
integerTok = myToken anyInteger

anyFloat (Float f) = Just f
anyFloat _ = Nothing

floatTok :: Parser Double
floatTok = myToken anyFloat

stringTok :: Parser String
stringTok = myToken anyString

blockStringTok :: Parser String
blockStringTok = myToken anyBlockString

anyChar' (Char c) = Just c
anyChar' _ = Nothing

charTok :: Parser Char
charTok = myToken anyChar'

anyBool (Bool b) = Just b
anyBool _ = Nothing

boolTok :: Parser Bool
boolTok = myToken anyBool

matchSymbol n (Symbol s) | n == s    = Just ()
                        | otherwise = Nothing
matchSymbol _ _ = Nothing                                    

symbolNamed s = myToken (matchSymbol s)

symbolChar = oneOf "()[]{}<>.;,"

surround :: Char -> Char -> Parser a -> Parser a
surround l r = between (symbolNamed l) (symbolNamed r)

parens = surround '(' ')'
braces = surround '{' '}'
squares = surround '[' ']'
angles = surround '<' '>'

anyString (String str) = Just str
anyString _ = Nothing

anyBlockString (BlockString str) = Just str
anyBlockString _ = Nothing

anyStringTok :: Parser String
anyStringTok = stringTok <|> blockStringTok

tokenizer :: P.Parser [SpanToken]
tokenizer = do 
  P.whiteSpace lexeme
  ts <- many (P.lexeme lexeme token)
  eof
  return ts

keywordL :: P.Parser String
keywordL = choice $ map (\ str -> P.reserved lexeme str >> return str) keywords

operator :: P.Parser String
operator =  choice (map (\ s  -> reservedOp s >> return s) wordOps) <|> many1 (oneOf opSymbol) 

charLex = do
  symbol "'"
  c <- anyChar
  symbol "'"
  return c

lexeme :: Stream s m Char => P.GenTokenParser s u m
lexeme = 
    P.makeTokenParser $ P.LanguageDef
         {
           P.commentStart   = "{-",
           P.commentEnd     = "-}",
           P.commentLine    = "--",
           P.nestedComments = True,
           P.identStart     = letter,
           P.identLetter    = alphaNum <|> oneOf "_'" ,
           P.opStart        = oneOf opSymbol,
           P.opLetter       = oneOf opSymbol,
           P.reservedOpNames = predefinedOps,
           P.reservedNames = keywords,
           P.caseSensitive = True
         }

wordOps = ["and then", "and", "or else", "or", "implies","xor"]

predefinedOps = concat [ ["*","+"]
                       , ["<=","=", "/="]
                       , ["<",">"]
                       , ["\"[","]\""]
                       , [":=","{","}"]
                       , wordOps
                       ]

keywords = concat [["True","False"]
                  ,["Void"]
                  ,["not", "old"]
                  ,["agent"]
                  ,["alias", "assign"]
                  ,["attached","as"]
                  ,["inspect", "when"]
                  ,["if","then","else","elseif"]
                  ,["from","until","loop"]
                  ,["is","do","end","once"]
                  ,["external", "obsolete"]
                  ,["built_in"]
                  ,["class","inherit","note"]
                  ,["check"]
                  ,["debug"]
                  ,["create", "convert"]
                  ,["Result", "Current"]
                  ,["Precursor"]
                  ,["top", "procs", "dot"]
                  ,["like", "detachable"]
                  ,["frozen","feature","local"]
                  ,["print","printd"]
                  ,["deferred", "attribute"]
                  ,["redefine", "rename"]
                  ,["ensure then", "require else", "ensure","require","invariant"]
                  ,["locks","require-order"]
                  ,["INTEGER","REAL","BOOLEAN"]
                  ,wordOps
                  ]


opSymbol = "!#$%&*+/<=>?@\\^|-~:"

bool :: P.Parser Bool
bool = try (string "True" >> return True) <|>
       try (string "False" >> return False)

symbol = P.symbol lexeme

identifierL :: Stream s m Char => ParsecT s u m String
identifierL = P.identifier lexeme

integer :: Stream s m Char => ParsecT s u m Integer
integer = P.integer lexeme

float :: Stream s m Char => ParsecT s u m Double
float = P.float lexeme

reservedOp :: Stream s m Char => String -> ParsecT s u m ()
reservedOp = P.reservedOp lexeme

stringLiteral :: Stream s m Char => ParsecT s u m String
stringLiteral = P.stringLiteral lexeme

-- anyString :: P.Parser String
-- anyString = blockString <|> stringLiteral

blockString :: P.Parser String
blockString = try (string "\"[") >> manyTill anyChar (symbol "]\"")