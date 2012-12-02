{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Eiffel.Parser.Lex 
       ( Parser
       , Token (..)
       , SpanToken (..)
       , keyword
       , identifier
       , squares
       , comma
       , identifierNamed
       , colon
       , parens
       , semicolon
       , period
       , symbol
       , braces
         
       , attachTokenPos
         
       , freeOperator
       , opNamed
       , opSymbol
         
       , tokenizer
       , tokenizeFile
         
       , charTok
       , stringTok
       , anyStringTok
       , integerTok
       , floatTok
       , boolTok
       )
  where
import Control.Monad.Identity
import Control.DeepSeq

import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as Strict
import qualified Data.Map as Map

import Language.Eiffel.Position

import Text.Parsec
import Text.Parsec.Pos
}

%wrapper "monad-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$paren = [\(\)\<\>\{\}\[\]]
$symbol = [\, \; \` \' \? \:]
$operator = [\=\<\>\$\&\|\+\-\/\~\*\.\#\^\\\@]
$graphic = $printable # $white
$stringinner = [$graphic '\ ' \t] # \"

@esc     = \% ($printable | \/ $digit+ \/)
@char = $printable | @esc
@eol = \r? \n
@blockstring = \" \[ @eol ($printable | $white)* \] \"
@string = \" ($stringinner | \%. | \%@eol)* \"

eiffel :-

<0>  $white+          ;
<0>  "--" .*          ;
<0>  \' @char \'      { withPos (Char . BS.head . BS.tail . BS.init) }
<0>  "and then"       { withPos (const TokAndThen) }
<0>  "or else"        { withPos (const TokOrElse) }
<0>  "ensure then"    { withPos (const TokEnsureThen) }
<0>  "require else"   { withPos (const TokRequireElse) }
<0>  $operator+       { withPos (Operator . BS.unpack) }
<0>  $paren           { withPos (Paren . BS.head) }
<0>  $digit+\.$digit+ { withPos (Float . read . BS.unpack) }
<0>  $digit+ (\_ $digit+)* { withPos (Integer . read . filter (/= '_') . BS.unpack ) }
<0>  $alpha[$alpha $digit \_ \']* { withPos lookupIdentifier }
<0>  $symbol          { withPos (Symbol . BS.head) }
<0>  @blockstring     { withPos (BlockString . BS.unpack . BS.tail . BS.init) }
<0>  @string          { withPos (String . BS.unpack . BS.tail . BS.init) }
<0>  eof              { withPos (tokConst EOF) }
<0>  .                { withPos (tokConst LexError) }

{

tokenMap :: Map.Map ByteString Token
tokenMap = 
  Map.fromList
   [("true",  TokTrue)
   ,("false", TokFalse)
   ,("void",  TokVoid )
   ,("and",   TokAnd)
   ,("or",    TokOr)
   ,("implies", TokImplies)
   ,("xor", TokXor)
   ,("not",   TokNot)
   ,("old",   TokOld)
   ,("agent", TokAgent)
   ,("alias", TokAlias)
   ,("assign",  TokAssign)
   ,("across",  TokAcross)
   ,("attached", TokAttached)
   ,("inspect", TokInspect)
   ,("when",    TokWhen)
   ,("if",      TokIf)              
   ,("then",    TokThen)
   ,("else",    TokElse)
   ,("elseif",  TokElseIf)
   ,("from",    TokFrom)
   ,("until",   TokUntil)
   ,("loop",    TokLoop)
   ,("variant", TokVariant)
   ,("is", TokIs)
   ,("do", TokDo)
   ,("end", TokEnd)
   ,("once", TokOnce)
   ,("retry", TokRetry) 
   ,("rescue", TokRescue)
   ,("external", TokExternal)
   ,("obsolete", TokObsolete)
   ,("built_in", TokBuiltin)
   ,("class", TokClass)
   ,("inherit", TokInherit)
   ,("note", TokNote)
   ,("check", TokCheck)
   ,("debug", TokDebug)
   ,("create", TokCreate)
   ,("convert", TokConvert)
   ,("result", TokResult)
   ,("current", TokCurrent)
   ,("precursor", TokPrecursor)
   ,("like", TokLike)
   ,("detachable", TokDetachable) 
   ,("separate", TokSeparate)
   ,("frozen", TokFrozen)
   ,("expanded", TokExpanded)
   ,("feature", TokFeature)
   ,("local", TokLocal)
   ,("deferred", TokDeferred) 
   ,("attribute", TokAttribute)
   ,("export", TokExport)
   ,("redefine", TokRedefine)
   ,("rename", TokRename)
   ,("select", TokSelect)
   ,("undefine", TokUndefine)
   ,("all", TokAll)
   ,("some", TokSome)
   ,("ensure", TokEnsure)
   ,("require", TokRequire)
   ,("invariant", TokInvariant)
   ,("as", TokAs)
   ]

tokConst :: a -> ByteString -> a
tokConst = const

lookupIdentifier :: ByteString -> Token
lookupIdentifier x = 
  let x' = BS.map toLower x
  in case Map.lookup x' tokenMap of
    Just t -> t
    Nothing -> Identifier (BS.unpack x)

data Token 
    = Identifier String
    | Symbol Char
    | String String
    | BlockString String
    | Char Char
    | Paren Char
    | Operator String
    | Float Double
    | Integer Integer
    | TokTrue
    | TokFalse
    | TokVoid
    | TokNot
    | TokOld
    | TokAgent
    | TokAlias
    | TokAssign
    | TokAcross
    | TokAttached
    | TokInspect
    | TokWhen
    | TokIf              
    | TokThen
    | TokElse
    | TokElseIf
    | TokFrom
    | TokUntil
    | TokLoop
    | TokVariant
    | TokIs
    | TokDo
    | TokEnd
    | TokOnce
    | TokRetry 
    | TokRescue
    | TokExternal
    | TokObsolete
    | TokBuiltin
    | TokClass
    | TokInherit
    | TokNote
    | TokCheck
    | TokDebug
    | TokCreate
    | TokConvert
    | TokResult
    | TokCurrent
    | TokPrecursor
    | TokLike
    | TokDetachable 
    | TokSeparate
    | TokFrozen
    | TokExpanded
    | TokFeature
    | TokLocal
    | TokDeferred 
    | TokAttribute
    | TokExport
    | TokRedefine
    | TokRename
    | TokSelect
    | TokUndefine
    | TokAll
    | TokSome
    | TokEnsure
    | TokEnsureThen
    | TokRequire
    | TokRequireElse
    | TokInvariant
    | TokAs
    | TokAnd
    | TokOr
    | TokImplies
    | TokXor
    | TokAndThen
    | TokOrElse
    | EOF
    | LexError
      deriving (Eq, Show)

withPos :: (ByteString -> Token) -> AlexInput -> Int -> Alex Token
withPos f (_, _, str) i 
  = return (f $ BS.take (fromIntegral i) str)

type Parser a = Parsec [SpanToken] () a

data SpanToken = 
  SpanToken { spanP     :: SourcePos
            , spanToken :: Token
            } 
  deriving Show

instance NFData Token where
  rnf !t = ()
  
instance NFData SpanToken where
  rnf (SpanToken !pos !tok) = ()

alexPosnToPos (AlexPn _ line col) = 
  newPos "FIXME: Lex.hs, no file"
         line
         col

grabToken :: (Token -> Maybe a) -> Parser a
grabToken f = Text.Parsec.token show spanP (f . spanToken)

opSymbol = "=<>$&|+-/~*.#^@"

symbolF s (Symbol sym)
  | sym == s = Just ()
  | otherwise = Nothing
symbolF _ _ = Nothing

identifier = grabToken f
  where f (Identifier i) = Just i
        f _ = Nothing

identifierNamed n = grabToken f
  where f (Identifier i) 
          | n == i = Just i
          | otherwise = Nothing
        f _ = Nothing

opNamed op = keyword (Operator op) >> return ()

period    = opNamed "."

symbol = grabToken . symbolF

semicolon = grabToken (symbolF ';')
comma     = grabToken (symbolF ',')
colon     = grabToken (symbolF ':')

langle  = keyword (Operator "<")
rangle  = keyword (Operator ">")
lbrak   = keyword (Paren '{')
rbrak   = keyword (Paren '}')
lparen  = keyword (Paren '(')
rparen  = keyword (Paren ')')
lsquare = keyword (Paren '[')
rsquare = keyword (Paren ']')

braces = between lbrak rbrak
angles = between langle rangle
squares = between lsquare rsquare
parens = between lparen rparen

attachTokenPos :: Parser a -> Parser (Pos a)
attachTokenPos p = do
  tks <- getInput
  case tks of
    t:_ -> attachPos (spanP t) `fmap` p
    _ -> fail "no more input"

freeOperator :: Parser String
freeOperator = grabToken nonFree <?> "free operator"
  where 
    nonFree (Operator op) | not (op `elem` predefinedOps) = Just op
    nonFree _ = Nothing
    
    wordOps = ["and then", "and", "or else", "or", "implies","xor"]

    predefinedOps = concat [ ["*","+","-", "^"]
                           , ["<=",">=","=","/=","~","/~"]
                           , ["<<", ">>"]
                           , ["<",">"]
                           , ["\"[","]\""]
                           , [":=","?=","{","}"]
                           , wordOps
                           ]

type AlexUserState = String

anyStringTok = stringTok <|> blockStringTok

blockStringTok = grabToken f
  where f (BlockString s) = Just s
        f _ = Nothing

stringTok = grabToken f
  where f (String s) = Just s
        f _ = Nothing

charTok = grabToken f
  where f (Char c) = Just c
        f _ = Nothing

boolTok = grabToken f
  where f TokTrue = Just True
        f TokFalse = Just False
        f _ = Nothing

integerTok = grabToken f
  where f (Integer i) = Just i
        f _ = Nothing

floatTok = grabToken f
  where f (Float d) = Just d
        f _ = Nothing


keyword t = grabToken f >> return ()
  where f t' 
          | t == t'   = Just t
          | otherwise = Nothing

alexInitUserState = "<nofile>"

ignorePendingBytes = id

alexEOF :: Alex Token
alexEOF = return EOF

runTokenizer file str = runAlex str $
  let loop ts = do 
        tok <- alexMonadScan
        (AlexPn _ line col, _, _) <- alexGetInput
        case tok of
          EOF -> return (reverse ts)
          LexError -> error ("lexer error: " ++ show (newPos file line col))
          _ -> loop (SpanToken (newPos file line col) tok:ts)
  in loop []

tokenizer :: String -> Strict.ByteString -> Either String [SpanToken]
tokenizer file bstr = runTokenizer file (BS.fromChunks [bstr])

tokenizeFile :: String -> IO (Either String [SpanToken])
tokenizeFile file = do
  s <- BS.readFile file
  return $ runTokenizer file s

}
