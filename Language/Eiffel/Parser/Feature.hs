{-# LANGUAGE KindSignatures #-}

module Language.Eiffel.Parser.Feature where

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Expr
import Language.Eiffel.Parser.Lex
import Language.Eiffel.Parser.Statement
import Language.Eiffel.Parser.Typ

import Text.Parsec

type FeatParser (body :: * -> *) exp = 
    Parser (body exp) -> Parser (AbsFeature body exp)

feature :: FeatParser body Expr
feature implP = do
  fr    <- option False (keyword "frozen" >> return True)
  name  <- identifier   <?> "Feature declaration identifier"

  als   <- optionMaybe alias

  args  <- argumentList <?> "Argument list"
  res   <- option NoType (opNamed ":" >> typ)
  optional (keyword "is")
  pGens <- option [] procGens

  reqLk <- option [] reqOrder
  ensLk <- option [] locks

  reqs  <- option [] requires
  impl  <- implP
  ens   <- option [] ensures

  keyword "end"

  return $ AbsFeature
             {
               featureFroz = fr
             , featureName = name
             , featureAlias  = als
             , featureArgs   = args
             , featureResult = res
             , featureProcs  = pGens
             , featureReq    = reqs
             , featureReqLk  = reqLk

             , featureImpl   = impl
             , featureEns    = ens
             , featureEnsLk  = ensLk
             }

clause :: Parser (Clause Expr)
clause = do 
  tag <- identifier
  opNamed ":"
  Clause tag `fmap` expr

alias = do
  keyword "alias"
  str <- stringTok
  als <- if all (flip elem opSymbol) str
         then return str
         else fail $ "unallowed alias symbol: " ++ str
  return als

obsolete :: Parser String
obsolete = keyword "obsolete" >> stringTok

requires :: Parser [Clause Expr]
requires = keyword "require" >> many clause

ensures :: Parser [Clause Expr]
ensures = keyword "ensure" >> many clause

reqOrder :: Parser [ProcExpr]
reqOrder = keyword "require-order" >> procExprP `sepBy` comma

locks :: Parser [Proc]
locks = keyword "lock" >> procGen `sepBy` comma

external :: Parser Stmt
external = attachTokenPos
           (do
             keyword "external"
             s <- stringTok
             if s == "built_in" 
               then return BuiltIn 
               else parserFail "only supporting built_in external for now"
           )


deferred = attachTokenPos $ do
  keyword "deferred"
  return BuiltIn

featureImplP :: Parser (FeatureBody Expr)
featureImplP = do
  optional (keyword "is")
  optional obsolete
  procs <- option [] (keyword "procs" >> many proc)
  decls <- option [] (keyword "local" >> many decl)
  body  <- try external <|> try deferred <|> featBody
  return (FeatureBody
          { featureLocal = decls
          , featureLocalProcs = procs
          , featureBody  = body
          }
         )

featBody :: Parser Stmt 
featBody = attachTokenPos $
           (keyword "do" <|> keyword "once") >> 
           Block `fmap` stmts
