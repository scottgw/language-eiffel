{-# LANGUAGE FlexibleContexts #-}
module Language.Eiffel.Parser.Expr (expr, call, var) where

import Control.Applicative ((<$>))
import Control.Monad.Identity (Identity)

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Lex

import Text.Parsec
import Text.Parsec.Expr

expr :: Parser Expr
expr = buildExpressionParser table factor

table :: OperatorTable [SpanToken] () Identity Expr
table = 
    [
     [lookupOp, dotOperator]
     ,[prefix (keyword "not") (UnOpExpr Not)
      ,prefix (opNamed "-")   (UnOpExpr Neg)
      ,prefix (keyword "sqrt") (UnOpExpr Sqrt)
      ,prefix (keyword "old") (UnOpExpr Old)
      ]
    ,[binaryOp "*"  (BinOpExpr Mul) AssocLeft
     ,binaryOp "/"  (BinOpExpr Div) AssocLeft]
    ,[binaryOp "+"  (BinOpExpr Add) AssocLeft
     ,binaryOp "-"  (BinOpExpr Sub) AssocLeft]
    ,[binaryOp "<=" (BinOpExpr (RelOp Lte NoType)) AssocLeft]
    ,[binaryOp "<"  (BinOpExpr (RelOp Lt  NoType)) AssocLeft]
    ,[binaryOp "="  (BinOpExpr (RelOp Eq  NoType)) AssocLeft]
    ,[binaryOp "~"  (BinOpExpr (RelOp TildeEq  NoType)) AssocLeft]
    ,[binaryOp "/=" (BinOpExpr (RelOp Neq NoType)) AssocLeft]
    ,[binaryOp ">"  (BinOpExpr (RelOp Gt  NoType)) AssocLeft]
    ,[binaryOp ">=" (BinOpExpr (RelOp Gte NoType)) AssocLeft]

    ,[
      binaryOp "and then"  (BinOpExpr Or)   AssocLeft             
     ,binaryOp "and"  (BinOpExpr And)  AssocLeft
     ,binaryOp "or else"  (BinOpExpr Or)   AssocLeft
     ,binaryOp "or"  (BinOpExpr Or)   AssocLeft
     ,binaryOp "implies"  (BinOpExpr Implies)   AssocLeft
     ]
    ,[otherOperator]
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
                r <- squares factor
                return ( \ target -> attachPos p $ BinOpExpr (SymbolOp "[]") target r))

-- Buggy, kills other parses, probably because it makes '(' an operator
otherOperator :: Operator [SpanToken] () Identity Expr
otherOperator = do
  Infix (do
          p <- getPosition
          op <- someOp
          return (\a b-> attachPos p (BinOpExpr (SymbolOp op) a b))
        ) AssocLeft



prefix :: Parser () -> (Expr -> UnPosExpr) -> Operator [SpanToken] () Identity Expr
prefix parseOp fun = 
    Prefix (do
             p <- getPosition
             parseOp
             return (\ a -> attachPos p (fun a))
           )

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
                     , agent
                     , question
                     , attached
                     , currentVar
                     , resultVar
                     , varOrCall 
                     , void
                     , contents <$> (parens expr)
                     ]

question = do
  opNamed "?"
  return (VarOrCall "?")

agent = do
  keyword "agent"
  contents <$> expr

varOrCall = do
  i <- identifier
  (UnqualCall i <$> argsP) <|> (return (VarOrCall i))

stringLit = LitString <$> stringTok
charLit = LitChar <$> charTok

attached :: Parser UnPosExpr
attached = do
  keyword "attached"
  cname <- braces identifier
  trg <- expr
  keyword "as"
  newName <- identifier
  return $ Attached cname trg newName

void :: Parser UnPosExpr
void = keyword "Void" >> return LitVoid

argsP = parens (expr `sepBy` comma)

isCall e | isCallUnPos (contents e) = return (contents e)
         | otherwise = fail "not a call"
    where
      isCallUnPos (QualCall _ _ _) = True
      isCallUnPos (UnqualCall _ _) = True
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
