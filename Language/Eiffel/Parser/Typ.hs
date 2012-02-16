module Language.Eiffel.Parser.Typ where

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Expr
import Language.Eiffel.Parser.Lex

import Text.Parsec

likeTyp :: Parser Typ
likeTyp = keyword "like" >> Like `fmap` (identifier <|> (keyword "Current" >> return "Current"))

intTyp :: Parser Typ
intTyp = keyword "INTEGER" >> return IntType

doubleTyp :: Parser Typ
doubleTyp = keyword "REAL" >> return DoubleType

boolTyp :: Parser Typ
boolTyp = keyword "BOOLEAN" >> return BoolType

classTyp :: Parser Typ
classTyp = do
  i  <- identifier
  gs <- option [] (squares (sepBy1 typ comma))
  return (ClassType i gs)

detTyp :: Parser Typ
detTyp = keyword "detachable" >> (likeTyp <|> baseTyp)

attTyp :: Parser Typ
attTyp = keyword "attached" >> (likeTyp <|> baseTyp)

typ :: Parser Typ
typ = detTyp <|> attTyp <|> likeTyp <|> sepTyp <|> baseTyp

baseTyp :: Parser Typ
baseTyp = intTyp <|> boolTyp <|> doubleTyp <|> classTyp

sepTyp :: Parser Typ
sepTyp = do
  keyword "separate"
  p   <- optionMaybe (angles procGen)
  ps  <- option [] procGens
  cn  <- identifier
  return $ Sep p ps cn

declEq :: Parser Decl
declEq = do
  d <- decl
  optional (opNamed "=" >> expr)
  return d

decl :: Parser Decl
decl = do
  name <- identifier <?> "Declaration identifier"
  decl' name

decl' :: String -> Parser Decl
decl' varName = do
  colon           <?> "Declaration ':'"
  typeName <- typ <?> "Declaration type"
  return $ Decl varName typeName

argumentList :: Parser [Decl]
argumentList = option [] (parens (decl `sepBy` semicolon))

dot :: Parser Proc
dot = keyword "dot" >> return Dot

procGen :: Parser Proc
procGen = dot <|> Proc `fmap` identifier

procGens :: Parser [Proc]
procGens = angles (sepBy procGen comma)

proc :: Parser ProcDecl
proc = do
  pg <- procGen
  colon
  subTop pg <|> lessProc pg

procExprs :: Parser [ProcExpr]
procExprs = angles (sepBy procExprP comma)

procExprP :: Parser ProcExpr
procExprP = do
  a <- procGen
  opNamed "<"
  b <- procGen
  return (LessThan a b)

subTop :: Proc -> Parser ProcDecl
subTop pg = do
  keyword "top"
  return $ SubTop pg

lessProc :: Proc -> Parser ProcDecl
lessProc pg = opNamed "<" >> fmap (CreateLessThan pg) procGen
