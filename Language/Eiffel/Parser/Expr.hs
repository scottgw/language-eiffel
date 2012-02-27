{-# LANGUAGE FlexibleContexts #-}
module Language.Eiffel.Parser.Expr (expr, call, var) where

import Control.Applicative ((<$>), (<*))
import Control.Monad.Identity (Identity)

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Lex
import Language.Eiffel.Parser.Typ
import {-# SOURCE #-} Language.Eiffel.Parser.Statement

import Text.Parsec
import Text.Parsec.Expr

expr :: Parser Expr
expr = buildExpressionParser table factor

table :: OperatorTable [SpanToken] () Identity Expr
table = 
    [ [ prefixes ]
    , [ otherOperator ]            
    , [ binaryOp "^"  (BinOpExpr Pow) AssocRight]
    , [ binaryOp "*"  (BinOpExpr Mul) AssocLeft
      , binaryOp "/"  (BinOpExpr Div) AssocLeft
      , binaryOp "//" (BinOpExpr Quot) AssocLeft
      , binaryOp "\\\\" (BinOpExpr Rem) AssocLeft
      ]
    , [ binaryOp "+"  (BinOpExpr Add) AssocLeft
      , binaryOp "-"  (BinOpExpr Sub) AssocLeft]
    , [ binaryOp "<=" (BinOpExpr (RelOp Lte NoType)) AssocLeft
      , binaryOp "<"  (BinOpExpr (RelOp Lt  NoType)) AssocLeft
      , binaryOp "="  (BinOpExpr (RelOp Eq  NoType)) AssocLeft
      , binaryOp "~"  (BinOpExpr (RelOp TildeEq  NoType)) AssocLeft
      , binaryOp "/=" (BinOpExpr (RelOp Neq NoType)) AssocLeft
      , binaryOp "/~"  (BinOpExpr (RelOp TildeNeq  NoType)) AssocLeft
      , binaryOp ">"  (BinOpExpr (RelOp Gt  NoType)) AssocLeft
      , binaryOp ">=" (BinOpExpr (RelOp Gte NoType)) AssocLeft
      ]
    , [ binaryOp "and then"  (BinOpExpr AndThen)   AssocLeft             
      , binaryOp "and"  (BinOpExpr And)  AssocLeft
      ] 
    , [ binaryOp "or else"  (BinOpExpr OrElse)   AssocLeft
      , binaryOp "or"  (BinOpExpr Or)   AssocLeft
      , binaryOp "xor"  (BinOpExpr Xor)   AssocLeft
      ]
    , [ binaryOp "implies"  (BinOpExpr Implies)   AssocLeft]
    ]

dotOperator 
    = Postfix (do
                p <- getPosition
                opNamed "."
                i <- identifier
                args <- option [] (parens (sepBy expr comma))
                return (\ target -> attachPos p $ QualCall target i args))

lookupOp
    = Postfix (do
                p <- getPosition
                r <- squares expr
                return ( \ target -> attachPos p $ BinOpExpr (SymbolOp "[]") target r))

-- Buggy, kills other parses, probably because it makes '(' an operator
otherOperator :: Operator [SpanToken] () Identity Expr
otherOperator = do
  Infix (do
          p <- getPosition
          op <- freeOperator
          return (\a b-> attachPos p (BinOpExpr (SymbolOp op) a b))
        ) AssocLeft

prefixes =  
  let 
    parseUnOp parseOp fun = do
      p <- getPosition
      parseOp
      return (\expr -> attachPos p (fun expr))
    op = choice [ parseUnOp (keyword "not") (UnOpExpr Not)
                , parseUnOp (keyword "old") (UnOpExpr Old)
                , parseUnOp (opNamed "-")   (UnOpExpr Neg)
                , parseUnOp (keyword "sqrt") (UnOpExpr Sqrt)
                ]
  in Prefix $ do 
    ops <- many1 op
    let combinedOp = foldr (.) id ops
    return combinedOp

binaryOp str = binary (opNamed str)
binaryKey str = binary (keyword str)

binary :: Parser () -> (Expr -> Expr -> UnPosExpr) -> Assoc -> Operator [SpanToken] () Identity Expr
binary binOp fun = 
    Infix (do
            p <- getPosition
            binOp
            return (\ a b -> attachPos p (fun a b))
          )

factor :: Parser Expr
factor = attachTokenPos factorUnPos

factorUnPos :: Parser UnPosExpr
factorUnPos = choice [ doubleLit
                     , intLit
                     , boolLit
                     , stringLit
                     , charLit
                     , tuple
                     , agent
                     , question
                     , attached
                     , createExpr
                     , varOrCall
                     , typeLit
                     , precursorCall
                     , void
                     ]

tuple = Tuple <$> squares (expr `sepBy` comma)

question = do
  opNamed "?"
  return (VarOrCall "?")

agent = do
  keyword "agent"
  inlineAgent <|> (Agent <$> expr)

inlineAgent = do
  argDecls <- argumentList
  resultType <- optionMaybe  (colon >> typ)
  keyword "do"
  stmts <- many stmt
  keyword "end"
  args <- option [] argsP
  return (InlineAgent argDecls resultType stmts args)

varOrCall =
  let identStart = do 
        i <- identifier
        (UnqualCall i <$> argsP) <|> return (VarOrCall i)
      specialStart = resultVar <|> currentVar 
      static = LitStaticClass <$> braces typ
      
      -- we try here because the parens could have a 
      -- non-type expression inside
      typeL = LitType <$> try (parens (braces typ))
  in do
    p <- getPosition
    t <- specialStart <|> identStart <|> static <|> 
            typeL <|> (contents <$> (parens expr))
    call' (attachPos p t)

call' :: Expr -> Parser UnPosExpr
call' targ = 
  let periodStart = do
        period
        i <- identifier
        p <- getPosition
        args <- option [] argsP
        call' (attachPos p $ QualCall targ i args)
      squareStart = do
        p <- getPosition
        e <- squares expr
        call' (attachPos p $ BinOpExpr (SymbolOp "[]") targ e)
  in periodStart <|> squareStart <|> return (contents targ)
precursorCall = do
  keyword "Precursor"
  cname <- optionMaybe (braces identifier)
  args <- option [] argsP
  return $ PrecursorCall cname args
  
stringLit = LitString <$> anyStringTok
charLit = LitChar <$> charTok
typeLit = LitType <$> (braces typ <* notFollowedBy period)

attached :: Parser UnPosExpr
attached = do
  keyword "attached"
  cname <- optionMaybe (braces typ)
  trg <- expr
  newName <- optionMaybe (keyword "as" >> identifier)
  return $ Attached cname trg newName
  
createExpr :: Parser UnPosExpr
createExpr = do
  keyword "create"
  t <- braces typ
  period
  i <- identifier
  args <- option [] argsP
  return $ CreateExpr t i args  

void :: Parser UnPosExpr
void = keyword "Void" >> return LitVoid

argsP = parens (expr `sepBy` comma)

isCall e | isCallUnPos (contents e) = return (contents e)
         | otherwise = fail "not a call"
    where
      isCallUnPos (QualCall _ _ _) = True
      isCallUnPos (UnqualCall _ _) = True
      isCallUnPos (PrecursorCall _ _) = True
      isCallUnPos (VarOrCall _) = True
      isCallUnPos _ = False

call :: Parser UnPosExpr
call = expr >>= isCall

varAttrCall = do
  i <- identifier
  notFollowedBy argsP
  return (VarOrCall i)

var :: Parser UnPosExpr
var = currentVar <|> resultVar <|> varAttrCall

resultVar :: Parser UnPosExpr
resultVar = keyword "Result" >> return ResultVar

currentVar :: Parser UnPosExpr
currentVar = keyword "Current" >> return CurrentVar

intLit :: Parser UnPosExpr
intLit = (LitInt . fromIntegral) <$> integerTok

doubleLit :: Parser UnPosExpr
doubleLit = LitDouble <$> floatTok

boolLit :: Parser UnPosExpr
boolLit = LitBool <$> boolTok
