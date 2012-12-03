{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Language.Eiffel.Parser.Expr (expr, call, var, manifest) where

import Control.Applicative ((<$>), (*>))
import Control.Monad.Identity (Identity)

import Language.Eiffel.Syntax
import Language.Eiffel.Parser.Lex
import {-# SOURCE #-} Language.Eiffel.Parser.Statement
import Language.Eiffel.Position
import Language.Eiffel.Parser.Typ

import Text.Parsec

expr :: Parser Expr
expr = expr' 0

expr' :: Int -> Parser Expr
expr' minPrec =
  let
    loop :: Expr -> Parser Expr
    loop result =
        let go = do 
              (op, prec, opAssoc) <- binOpToken minPrec 
              let nextMinPrec = case opAssoc of
                    AssocLeft -> prec + 1
                    _         -> prec
              rhs <- expr' nextMinPrec
              result' <- attachTokenPos (return $ BinOpExpr op result rhs)
              loop result'
        in go <|> return result
  in factor >>= loop

factor :: Parser Expr
factor = attachTokenPos factorUnPos

factorUnPos :: Parser UnPosExpr
factorUnPos = choice [ tuple
                     , onceString
                     , address
                     , agent
                     , across
                     , question
                     , attached
                     , createExpr
                     , varOrCall
                     , precursorCall
                     , void
                     , manifest
                     , unaryExpr
                     ]

unaryExpr =
  let 
    notP = do 
      keyword TokNot
      UnOpExpr Not <$> factor
    oldP = do
      keyword TokOld
      UnOpExpr Old <$> factor
    negP = do
      opInfo Sub
      UnOpExpr Neg <$> factor
  in notP <|> oldP <|> negP

onceString = do
  keyword TokOnce
  s <- anyStringTok
  return (OnceStr s)

address = do
  opNamed "$"
  p <- getPosition
  e <- VarOrCall <$> identifier <|> resultVar <|> currentVar
  return (Address $ attachPos p e)

manifest = choice [ doubleLit
                  , intLit
                  , boolLit
                  , stringLit
                  , charLit
                  , arrayLit
                  , typeLitOrManifest
                  ]   

arrayLit = do
  opNamed "<<"
  elems <- expr `sepBy` comma
  opNamed ">>"
  return (LitArray elems)

across = do
  keyword TokAcross
  e <- expr
  keyword TokAs
  i <- identifier
  quant <- (keyword TokAll *> return All) <|> (keyword TokSome *> return Some)
  body <- expr
  keyword TokEnd
  return (AcrossExpr e i quant body)

tuple = Tuple <$> squares (expr `sepBy` comma)

question = do
  symbol '?'
  return (VarOrCall "?")

agent = do
  keyword TokAgent
  p <- getPosition
  inlineAgent <|> (Agent <$> attachPos p <$> varOrCall)

inlineAgent = do
  argDecls <- try argumentList
  resultType <- optionMaybe  (colon >> typ)
  keyword TokDo
  stmts <- many stmt
  keyword TokEnd
  args <- option [] argsP
  return (InlineAgent argDecls resultType stmts args)

varOrCall =
  let identStart = do 
        i <- identifier
        (UnqualCall i <$> argsP) <|> return (VarOrCall i)
      specialStart = resultVar <|> currentVar 
      
      bracketCall = do
        p <- getPosition
        t <- manifest <|> tuple
        call' (attachPos p t)
  in do
    p <- getPosition
    t <- specialStart <|> identStart <|> try staticCall <|> 
         (contents <$> (parens expr)) <|> bracketCall
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
        es <- squares (expr `sepBy` comma)
        call' (attachPos p $ Lookup targ es)
  in periodStart <|> squareStart <|> return (contents targ)
precursorCall = do
  keyword TokPrecursor
  cname <- optionMaybe (braces identifier)
  args <- option [] argsP
  return $ PrecursorCall cname args

staticCall = do
  t <- braces typ
  period
  i <- identifier
  args <- option [] argsP
  return $ StaticCall t i args

stringLit = LitString <$> anyStringTok
charLit = LitChar <$> charTok

typeLitOrManifest = do
  t <- braces typ
  p <- getPosition
  ManifestCast t <$> attachPos p <$> manifest <|> return (LitType t)

attached :: Parser UnPosExpr
attached = do
  keyword TokAttached
  cname <- optionMaybe (braces typ)
  trg <- expr
  newName <- optionMaybe (keyword TokAs >> identifier)
  return $ Attached cname trg newName
  
createExpr :: Parser UnPosExpr
createExpr = do
  keyword TokCreate
  t <- braces typ
  (i, args) <- (do period
                   i <- identifier
                   args <- option [] argsP
                   return (i, args)) <|> return (defaultCreate, [])
  return $ CreateExpr t i args  

void :: Parser UnPosExpr
void = keyword TokVoid >> return LitVoid

argsP = parens (expr `sepBy` comma)

isCall e | isCallUnPos (contents e) = return (contents e)
         | otherwise = fail "not a call"
    where
      isCallUnPos (QualCall _ _ _) = True
      isCallUnPos (UnqualCall _ _) = True
      isCallUnPos (PrecursorCall _ _) = True
      isCallUnPos (VarOrCall _) = True
      isCallUnPos (StaticCall _ _ _) = True
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
resultVar = keyword TokResult >> return ResultVar

currentVar :: Parser UnPosExpr
currentVar = keyword TokCurrent >> return CurrentVar

intLit :: Parser UnPosExpr
intLit = LitInt <$> integerTok

doubleLit :: Parser UnPosExpr
doubleLit = LitDouble <$> floatTok

boolLit :: Parser UnPosExpr
boolLit = LitBool <$> boolTok
