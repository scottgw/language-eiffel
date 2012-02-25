{-# LANGUAGE ScopedTypeVariables #-}

module Language.Eiffel.Parser.Statement where

import Language.Eiffel.Eiffel


import Language.Eiffel.Parser.Clause
import Language.Eiffel.Parser.Expr
import Language.Eiffel.Parser.Lex
import Language.Eiffel.Parser.Typ

import Text.Parsec

-- stmt :: Parser Stmt
stmt = attachTokenPos bareStmt

-- bareStmt :: Parser UnPosStmt
bareStmt = do -- choice [assign, create, ifStmt, printD, loop, printStmt]
     s <- choice [ printStmt
                 , assign
                 , check
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


inspect = 
  let whenPart = do 
        keyword "when"
        e <- expr
        s <- attachTokenPos (keyword "then" >> Block `fmap` stmts)
        return (e,s)
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
  keyword "end"
  return $ Check clauses

block :: Parser UnPosStmt
block = fmap Block stmts

ifStmt :: Parser UnPosStmt
ifStmt = do
  b  <- keyword "if" >> expr
  s1 <- attachTokenPos (keyword "then" >> fmap Block stmts)
  s2 <- attachTokenPos (option (Block []) elsePart)
  keyword "end"
  return (If b s1 s2)

elsePart :: Parser UnPosStmt
elsePart = ifelseP <|> elseP

elseP :: Parser UnPosStmt
elseP = keyword "else">> fmap Block stmts

ifelseP :: Parser UnPosStmt
ifelseP = do
  b <- keyword "elseif" >> expr
  s1 <- attachTokenPos $ keyword "then" >> fmap Block stmts
  s2 <- attachTokenPos $ option (Block []) elsePart
  return (If b s1 s2)

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
       ) <|> return (DefCreate t v)
  return s

loop :: Parser UnPosStmt
loop = do
  keyword "from"
  fr <- attachTokenPos block
  invarMb <- option [] (keyword "invariant" >> many clause)
  un <- keyword "until" >> expr
  lo <- attachTokenPos $ keyword "loop" >> block
  keyword "end"
  return (Loop fr invarMb un lo)

assignId :: Parser Expr
assignId = do
  i <- attachTokenPos var
  opNamed ":="
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
  
debug :: Parser UnPosStmt
debug = do
  keyword "debug"
  str <- option [] (parens anyStringTok)
  b <- attachTokenPos block
  keyword "end"
  return (Debug str b)  

printStmt :: Parser UnPosStmt
printStmt = do
  keyword "print"
  e <- parens expr
  return (Print e)

printD :: Parser UnPosStmt
printD = do
  keyword "printd"
  e <- parens expr
  return (PrintD e)