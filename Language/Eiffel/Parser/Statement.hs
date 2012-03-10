{-# LANGUAGE ScopedTypeVariables #-}

module Language.Eiffel.Parser.Statement where

import Language.Eiffel.Syntax

import Language.Eiffel.Parser.Clause
import Language.Eiffel.Parser.Expr
import Language.Eiffel.Parser.Lex
import Language.Eiffel.Parser.Typ

import Text.Parsec

-- stmt :: Parser Stmt
stmt = attachTokenPos bareStmt

-- bareStmt :: Parser UnPosStmt
bareStmt = do
     s <- choice [ printStmt
                 , across
                 , assign
                 , assignAttempt
                 , check
                 , retry
                 , create
                 , ifStmt
                 , inspect
                 , printD
                 , loop
                 , debug
                 , try callStmt
                 ]
     optional semicolon
     return s
stmts :: Parser [Stmt]
stmts = many stmt

stmts' = many bareStmt

retry = do
  keyword "retry"
  return Retry

across = do
  keyword "across"
  e <- expr
  keyword "as"
  i <- identifier
  keyword "loop"
  bl <- blockPos
  keyword "end"
  return (Across e i bl)

inspect = 
  let whenPart = do 
        keyword "when"
        es <- expr `sepBy1` comma
        s <- attachTokenPos (keyword "then" >> Block `fmap` stmts)
        return (es, s)
  in do
    keyword "inspect"
    e <- expr
    whens  <- many1 whenPart
    elseMb <- optionMaybe (attachTokenPos $ keyword "else" >> Block `fmap` stmts)
    keyword "end"
    return $ Inspect e whens elseMb

check = do
  keyword "check"
  clauses <- many clause
  let chk = keyword "end" >> return (Check clauses)
      checkBlock = do
        keyword "then"
        body <- blockPos
        keyword "end"
        return (CheckBlock clauses body)
  checkBlock <|> chk


blockPos = attachTokenPos block

block :: Parser UnPosStmt
block = fmap Block stmts

ifStmt :: Parser UnPosStmt
ifStmt = do
  b  <- keyword "if" >> expr
  body <- attachTokenPos (keyword "then" >> fmap Block stmts)
  ifelses <- many ifelseP
  elseMb <- optionMaybe elseP
  elseMb' <- maybe (return Nothing) (fmap Just . attachTokenPos . return) elseMb
  keyword "end"
  return (If b body ifelses elseMb')

-- elsePart :: Parser UnPosStmt
-- elsePart = ifelseP <|> elseP

elseP :: Parser UnPosStmt
elseP = keyword "else">> fmap Block stmts

ifelseP :: Parser (ElseIfPart Expr)
ifelseP = do
  b <- keyword "elseif" >> expr
  s1 <- attachTokenPos $ keyword "then" >> fmap Block stmts
  -- s2 <- attachTokenPos $ option (Block []) elsePart
  return (ElseIfPart b s1)

create :: Parser UnPosStmt
create = do
  keyword "create"
  t <- optionMaybe (braces typ)
  v <- attachTokenPos var
  s <- (do
         period
         callE <- call
         case callE of
           UnqualCall fName args -> return (Create t v fName args)
           VarOrCall fName -> return (Create t v fName [])
           e -> error $ "create: should not have parsed " ++ show e
       ) <|> return (Create t v defaultCreate [])
  return s

loop :: Parser UnPosStmt
loop = do
  keyword "from"
  fr <- attachTokenPos block
  invarMb <- option [] (keyword "invariant" >> many clause)
  un <- keyword "until" >> expr
  lo <- attachTokenPos $ keyword "loop" >> block
  var <- optionMaybe (keyword "variant" >> expr)
  keyword "end"
  return (Loop fr invarMb un lo var)

assignId :: Parser Expr
assignId = do
  e <- expr
  opNamed ":="
  return e
  
assignAttemptId :: Parser Expr
assignAttemptId = do
  i <- attachTokenPos var
  opNamed "?="
  return i  

callStmt :: Parser UnPosStmt
callStmt = do
  c <- attachTokenPos call
  return $ CallStmt c

assign :: Parser UnPosStmt
assign = do
  i <- try assignId
  e <- expr <?> "assignment expression"
  return $ Assign i e
  
assignAttempt :: Parser UnPosStmt
assignAttempt = do
  i <- try assignAttemptId
  e <- expr <?> "assignment attempt expression"
  return $ AssignAttempt i e  
  
debug :: Parser UnPosStmt
debug = do
  keyword "debug"
  str <- option [] (parens anyStringTok)
  b <- attachTokenPos block
  keyword "end"
  return (Debug str b)  

printStmt :: Parser UnPosStmt
printStmt = do
  keyword "print_i"
  e <- parens expr
  return (Print e)

printD :: Parser UnPosStmt
printD = do
  keyword "print_d"
  e <- parens expr
  return (PrintD e)