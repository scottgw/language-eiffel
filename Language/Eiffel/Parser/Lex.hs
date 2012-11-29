{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Language.Eiffel.Parser.Lex (Token (..),
                   SpanToken (..),
                   Parser,
                   tokenizer,
                   opSymbol,
                   symbolNamed,
                   attachTokenPos,
                   justToken,
                   opNamed,
                   someOp,
                   freeOperator,
                   someKeyword,
                   identifier,
                   identifierNamed,
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

import Control.Applicative ((<$>), (*>), (<*))
import Control.Monad.Identity

import Data.Char
import Data.ByteString (ByteString)
import Data.List (nub, sort)

import Text.Parsec hiding (token)
import qualified Text.Parsec as P (token)
import qualified Text.Parsec.ByteString as P
import qualified Text.Parsec.Token as P

import Language.Eiffel.Position

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

freeOperator :: Parser String
freeOperator = 
  let nonFree (Operator op) | not (op `elem` predefinedOps) = Just op
      nonFree _ = Nothing
  in myToken nonFree <?> "free operator"
  
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

identifierNamed str = 
  let ident (Identifier i) | i == str = Just i
      ident _ = Nothing 
  in myToken ident <?> ("'" ++ str ++ "'")

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
boolTok = myToken anyBool <?> "True or False"

matchSymbol n (Symbol s) | n == s    = Just ()
                        | otherwise = Nothing
matchSymbol _ _ = Nothing                                    

symbolNamed s = myToken (matchSymbol s) <?> [s]

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
  whiteSpace
  ts <- many (lexeme token)
  eof
  return ts

keywordL :: P.Parser String
keywordL = choice $ map (\ str -> reserved str >> return str) keywords

operator :: P.Parser String
operator =  choice (map (\ s -> reservedOp s >> return s) wordOps) <|> 
            (do c  <- oneOf opSymbol
                cs <- many (oneOf ('.':opSymbol))
                return (c:cs))

eiffelCharToCode c = 
  case lookup c ops of
    Just i -> return i
    Nothing -> fail [c]
    where ops = [ ('U', 0)
                , ('N', 10)
                , ('R', 13)
                , ('T', 9)
                , ('"', 34)
                ]

charLex = do
  char '\''
  c <- (char '%' *> escapeCode) <|> space <|> anyChar
  symbol "'"
  return c

escapeCode :: ParsecT ByteString () Identity Char
escapeCode = do
  i <- (char '/' *> integer <* char '/') <|>
       (eiffelCharToCode =<< anyChar)
  return (chr $ fromIntegral i)

{- # INLINE lang #-}
-- lang :: P.GenTokenParser ByteString () Identity
-- lang = 
--     P.makeTokenParser $ P.LanguageDef
--          {
--            P.commentStart   = "{-",
--            P.commentEnd     = "-}",
--            P.commentLine    = "--",
--            P.nestedComments = True,
--            P.identStart     = letter,
--            P.identLetter    = alphaNum <|> oneOf "_'" ,
--            P.opStart        = oneOf opSymbol,
--            P.opLetter       = oneOf opSymbol,
--            P.reservedOpNames = predefinedOps,
--            P.reservedNames = keywords,
--            P.caseSensitive = False
--          }

type TokParser = ParsecT ByteString () Identity

identStart     = letter
identLetter    = alphaNum <|> oneOf "_'"

opStart :: TokParser Char
opStart        = oneOf opSymbol
opLetter       = oneOf opSymbol
reservedOpNames = predefinedOps
reservedNames = keywords


reservedOp name =
  lexeme $ try $
    do{ string name
      ; notFollowedBy opLetter <?> ("end of " ++ show name)
      }

wordOps = ["and then", "and", "or else", "or", "implies","xor"]

predefinedOps = concat [ ["*","+","-"]
                       , ["<=",">=","=","/=","~","/~"]
                       , ["<<", ">>"]
                       , ["<",">"]
                       , ["\"[","]\""]
                       , [":=","?=","{","}"]
                       , wordOps
                       ]

keywords = concat [["True","False"]
                  ,["Void"]
                  ,["not", "old"]
                  ,["agent"]
                  ,["alias", "assign"]
                  ,["across"]
                  ,["attached","as"]
                  ,["inspect", "when"]
                  ,["if","then","else","elseif"]
                  ,["from","until","loop","variant"]
                  ,["is","do","end","once"]
                  ,["retry", "rescue"]
                  ,["external", "obsolete"]
                  ,["built_in"]
                  ,["class","inherit","note"]
                  ,["check"]
                  ,["debug"]
                  ,["create", "convert"]
                  ,["Result", "Current"]
                  ,["Precursor"]
                  ,["top", "procs", "dot_proc"]
                  ,["like", "detachable", "separate"]
                  ,["frozen","expanded","feature","local"]
                  ,["print_i","print_d"]
                  ,["deferred", "attribute"]
                  ,["export", "redefine", "rename", "select", "undefine"]
                  ,["all", "some"]
                  ,["ensure then", "require else", "ensure","require","invariant"]
                  ,["locks","require-order"]
                  ,wordOps
                  ]
symbol name = lexeme (string name)

lexeme p
  = do{ x <- p; whiteSpace; return x  }


--whiteSpace
whiteSpace :: ParsecT ByteString () Identity ()
whiteSpace = skipMany (simpleSpace <|> oneLineComment <?> "")
  where
    simpleSpace =
      skipMany1 (satisfy isSpace)

    commentLine = "--"
    commentStart = ""
    commentEnd = ""

    oneLineComment =
      do{ try (string  commentLine)
        ; skipMany (satisfy (/= '\n'))
        ; return ()
        }
    
    multiLineComment =
        do { try (string commentStart)
           ; inComment
           }

    inComment = inCommentMulti
--        | otherwise                = inCommentSingle

    inCommentMulti :: ParsecT ByteString () Identity ()
    inCommentMulti
        =   do{ try (string commentEnd) ; return () }
        <|> do{ multiLineComment                     ; inCommentMulti }
        <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
        <|> do{ oneOf startEnd                       ; inCommentMulti }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd ++ commentStart)

    inCommentSingle :: ParsecT ByteString () Identity ()
    inCommentSingle
        =   do{ try (string commentEnd); return () }
        <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
        <|> do{ oneOf startEnd                      ; inCommentSingle }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd ++ commentStart)

reserved name =
  lexeme $ try $
    do{ caseString name
      ; notFollowedBy identLetter <?> ("end of " ++ show name)
      }
                  
caseString :: String -> TokParser String
caseString name = do{ walk name; return name }
  where
    walk = mapM_ (\c -> caseChar c <?> msg)
    caseChar c
      | isAlpha c  = satisfy (\c' -> c' == toLower c || c' == toUpper c)
      | otherwise  = char c
                               
    msg         = show name
    
identifierL =
  lexeme $ try $
    do{ name <- ident
      ; if (isReservedName name)
        then unexpected ("reserved word " ++ show name)
        else return name
      }
                                                                                                                                                                                                                                                                 

ident
  = do{ c <- identStart
      ; cs <- many identLetter
      ; return (c:cs)
      }
    <?> "identifier"
    
isReservedName name
  = isReserved theReservedNames caseName
  where
    caseName = map toLower name

                                              
isReserved names name = scan names
  where
    scan []       = False
    scan (r:rs)   = case (compare r name) of
      LT  -> scan rs
      EQ  -> True
      GT  -> False

theReservedNames = sort . map (map toLower) $ reserved
  where
    reserved = reservedNames

opSymbol = "!#$%&*+/<=>?@\\^|-~:"

bool :: P.Parser Bool
bool = try (reserved "True" >> return True) <|>
       try (reserved "False" >> return False)

-- general defs copied again from Parsec.Token
integer :: ParsecT ByteString () Identity Integer
integer = lexeme int
  where
    int = do{ f <- lexeme sign
            ; n <- nat
            ; return (f n)
            }
    sign = (char '-' >> return negate)
           <|> (char '+' >> return id)
           <|> return id
    nat = zeroNumber <|> decimal

    zeroNumber = do{ char '0'
                   ; hexadecimal <|> octal <|> decimal <|> return 0
                   }
                 <?> ""
    decimal = number 10 digit
    hexadecimal = do{ oneOf "xX"; number 16 hexDigit }
    octal = do{ oneOf "oO"; number 8 octDigit  }
    number base baseDigit
        = do{ digits <- many1 (optional (char '_') >> baseDigit)
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }
-- end copy from Parsec.Token
float           = lexeme floating   <?> "float"     
                                                                   
-- floats
floating        = do{ n <- decimal
                    ; fractExponent n
                    }
                  
                  
natFloat        = do{ char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat
                                                                                                               
zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                     ; return (Left n)
                     }
                   <|> decimalFloat
                   <|> fractFloat 0
                   <|> return (Left 0)
  
decimalFloat    = do{ n <- decimal
                    ; option (Left n)
                      (fractFloat n)
                    }
                  
fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }
                  
fractExponent n = do{ fract <- fraction
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                  <|>
                  do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }
                  
fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                  <?> "fraction"
  where
    op d f    = (f + fromIntegral (digitToInt d))/10.0
                
exponent'       = do{ oneOf "eE"
                    ; f <- sign
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                  <?> "exponent"
  where
    power e  | e < 0      = 1.0/power(-e)
             | otherwise  = fromInteger (10^e)



int             = do{ f <- lexeme sign
                    ; n <- nat
                    ; return (f n)
                    }

sign            =   (char '-' >> return negate)
                    <|> (char '+' >> return id)
                    <|> return id
  
nat             = zeroNumber <|> decimal
                      
zeroNumber      = do{ char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                  <?> ""

decimal         = number 10 digit
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
octal           = do{ oneOf "oO"; number 8 octDigit  }
                                    
number base baseDigit
  = do{ digits <- many1 baseDigit
      ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
      ; seq n (return n)
      }

-- copied and adapted from the Parsec token parser generator
stringLiteral :: ParsecT ByteString () Identity String
stringLiteral = ((do 
  str <- between (char '"')
                 (char '"' <?> "end of string")
                 (many (stringChar stringLetter))
  return (foldr (maybe id (:)) "" str)) <?> "literal string") <* whiteSpace

stringChar :: ParsecT ByteString () Identity Char -> ParsecT ByteString () Identity (Maybe Char)
stringChar letterP = 
  (Just <$> letterP) <|> stringEscape <?> "string character"

stringLetter :: ParsecT ByteString () Identity Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '%') && (c > '\026'))


blockStringLetter :: ParsecT ByteString () Identity Char
blockStringLetter = 
  satisfy (\c -> (c /= '%' && c >= ' ' && c <= '~') || 
                 c == '\t' || c == '\n' || c == '\r')
                  -- c == '"' || c == '\r'))


stringEscape :: ParsecT ByteString () Identity (Maybe Char)
stringEscape = do char '%'
                  (escapeGap *> return Nothing) <|> (Just <$> escapeCode)

escapeGap :: ParsecT ByteString () Identity Char
escapeGap = do many1 space
               char '%' <?> "end of string gap"
                 
-- stringLiteral :: Stream s m Char => ParsecT ByteString () Identity String
-- stringLiteral = P.stringLiteral lang

-- anyString :: P.Parser String
-- anyString = blockString <|> stringLiteral

blockString :: P.Parser String
blockString = 
  let blockOpener = 
        try (string "\"[\n" >> return "\n") <|> 
        try (string "\"[\r\n" >> return "\r\n")
      blockCloser = 
        try (char '\n' >> many (char '\t') >>string "]\"")
  in do
    pre <- blockOpener
    chars' <- manyTill (stringChar blockStringLetter) blockCloser
    let chars = foldr (maybe id (:)) "" chars'
    return (pre ++ chars ++ "\n")

-- blockString :: Stream s m Char => ParsecT s u m String
-- blockString = ((do 
--   str <- between (try (string "')
--                  (char '"' <?> "end of string")
--                  (many stringChar)
--   return (foldr (maybe id (:)) "" str)) <?> "literal string") <* P.whiteSpace lang
