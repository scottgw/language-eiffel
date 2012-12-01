{
module Language.Eiffel.Parser.Lex where

import Language.Eiffel.Position

import Control.Monad.Identity
import Data.Char
import qualified Data.Map as Map
import System.Environment
import Text.Parsec
import Text.Parsec.Pos
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$paren = [\(\)\<\>\{\}\[\]]
$symbol = [\, \; \` \' \? \:]
$operator = [\=\<\>\$\&\|\+\-\/\~\*\.]
$graphic = $printable # $white

@string = \" (([$graphic '\ '] # \") | \\.)* \"

tokens :-

  $white+          ;
  "--" .*          ;
  "'"$graphic"'"   { withPos (Char . head . tail . init) }
  "and then"       { withPos Operator }
  "or else"        { withPos Operator }
  "ensure then"    { withPos (const TokEnsureThen) }
  "require else"   { withPos (const TokRequireElse) }
  $paren           { withPos (Paren . head) }
  $digit+\.$digit+ { withPos (Float . read) }
  $digit+          { withPos (Integer . read) }
  $alpha[$alpha $digit \_ \']* { withPos lookupIdentifier }
  $operator        { withPos Operator }
  $symbol          { withPos (Symbol . head) }
  @string          { withPos (String . tail . init) }
{

tokenMap :: Map.Map String Token
tokenMap = 
  Map.fromList
   [("true",  TokTrue)
   ,("false", TokFalse)
   ,("void",  TokVoid )
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
   ,("feature", Tokfeature)
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

lookupIdentifier x = 
  let x' = map toLower x
  in case Map.lookup x' tokenMap of
    Just t -> t
    Nothing -> Identifier x

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
    | Tokfeature
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
      deriving (Eq, Show)

withPos f p s = SpanToken p (f s)

type Parser a = Parsec [SpanToken] () a

data SpanToken = 
  SpanToken { spanP     :: AlexPosn
            , spanToken :: Token
            } 
  deriving Show


alexPosnToPos (AlexPn _ line col) = 
  newPos "FIXME: Lex.hs, no file"
         line
         col

grabToken :: (Token -> Maybe a) -> Parser a
grabToken f = token show (alexPosnToPos . spanP) (f . spanToken)

opSymbol = "=<>$&|+-/~*."

symbol s (Symbol sym)
  | sym == s = Just ()
  | otherwise = Nothing
symbol _ _ = Nothing

identifier = grabToken f
  where f (Identifier i) = Just i
        f _ = Nothing

identifierNamed n = grabToken f
  where f (Identifier i) 
          | n == i = Just i
          | otherwise = Nothing
        f _ = Nothing

opNamed op = keyword (Operator op) >> return ()

period    = grabToken (symbol '.')

semicolon = grabToken (symbol ';')
comma     = grabToken (symbol ',')
colon     = grabToken (symbol ':')

langle  = keyword (Paren '<')
rangle  = keyword (Paren '>')
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
    t:_ -> attachPos (alexPosnToPos $ spanP t) `fmap` p
    _ -> fail "no more input"

freeOperator :: Parser String
freeOperator = grabToken nonFree <?> "free operator"
  where 
    nonFree (Operator op) | not (op `elem` predefinedOps) = Just op
    nonFree _ = Nothing
    
    wordOps = ["and then", "and", "or else", "or", "implies","xor"]

    predefinedOps = concat [ ["*","+","-"]
                           , ["<=",">=","=","/=","~","/~"]
                           , ["<<", ">>"]
                           , ["<",">"]
                           , ["\"[","]\""]
                           , [":=","?=","{","}"]
                           , wordOps
                           ]

stringTok = anyStringTok

anyStringTok = grabToken f
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


keyword t = grabToken f
  where f t' 
          | t == t'   = Just t
          | otherwise = Nothing

lexFile file = do
  s <- readFile file
  return (alexScanTokens s)

}