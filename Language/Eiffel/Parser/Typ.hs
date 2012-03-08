module Language.Eiffel.Parser.Typ where

import Control.Applicative ((<$>))

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Lex

import Text.Parsec

likeTyp :: Parser Typ
likeTyp = keyword "like" >> Like `fmap` (identifier <|> (keyword "Current" >> return "Current"))

intTyp :: Parser Typ
intTyp = identifierNamed "INTEGER" >> return IntType

doubleTyp :: Parser Typ
doubleTyp = identifierNamed "REAL" >> return DoubleType

boolTyp :: Parser Typ
boolTyp = identifierNamed "BOOLEAN" >> return BoolType

classTyp :: Parser Typ
classTyp = do
  i  <- identifier
  gs <- option [] (squares (typ `sepBy1` comma))
  return (ClassType i gs)

tupleTyp :: Parser Typ
tupleTyp = do
  i <- identifierNamed "TUPLE"
  let typeDeclP =
        Right <$> concat <$> try (decl `sepBy1` semicolon) <|>
        Left <$> (typ `sepBy1` comma)
  typeOrDecls <- option (Left []) (squares typeDeclP)
  return (TupleType typeOrDecls)
  

detTyp :: Parser Typ
detTyp = keyword "detachable" >> (sepTyp <|> likeTyp <|> baseTyp)

attTyp :: Parser Typ
attTyp = keyword "attached" >> (likeTyp <|> baseTyp)

typ :: Parser Typ
typ = detTyp <|> attTyp <|> likeTyp <|> sepTyp <|> baseTyp

baseTyp :: Parser Typ
baseTyp = intTyp <|> boolTyp <|> doubleTyp <|> tupleTyp <|> classTyp

sepTyp :: Parser Typ
sepTyp = do
  keyword "separate"
  p   <- optionMaybe (angles procGen)
  ps  <- option [] procGens
  cn  <- identifier
  return $ Sep p ps cn

decl :: Parser [Decl]
decl = do
  names <- identifier `sepBy1` comma <?> "Declaration identifier"
  decl' names

decl' :: [String] -> Parser [Decl]
decl' varNames = do
  colon           <?> "Declaration ':'"
  typeName <- typ <?> "Declaration type"
  return $ map (flip Decl typeName) varNames

argumentList :: Parser [Decl]
argumentList = 
  option [] (concat `fmap` parens (decl `sepBy` optional semicolon))

dot :: Parser Proc
dot = keyword "dot_proc" >> return Dot

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
