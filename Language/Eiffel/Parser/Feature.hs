{-# LANGUAGE KindSignatures #-}

module Language.Eiffel.Parser.Feature where

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Clause
import Language.Eiffel.Parser.Lex
import Language.Eiffel.Parser.Statement
import Language.Eiffel.Parser.Typ

import Text.Parsec

type FeatParser (body :: * -> *) exp = 
    Parser (body exp) -> Parser (AbsRoutine body exp)

data FeatureHead =
  FeatureHead 
  { fHeadFrozen :: Bool
  , fHeadName :: String
  , fHeadAlias :: Maybe String
  , fHeadArgs :: [Decl]
  , fHeadRes :: Typ
  }

featureHead = do
  fr    <- option False (keyword "frozen" >> return True)
  name  <- identifier   <?> "Feature declaration identifier"

  als   <- optionMaybe alias

  args  <- argumentList <?> "Argument list"
  res   <- option NoType (colon >> typ)
  optional (keyword "is")
  optional obsolete

  return (FeatureHead fr name als args res)

routine :: FeatureHead -> Maybe String -> [Note] -> (Contract Expr) -> FeatParser body Expr
routine fHead assgn notes reqs implP  = do
  let FeatureHead fr name als args res = fHead

  pGens <- option [] procGens

  reqLk <- option [] reqOrder
  ensLk <- option [] locks

  impl  <- implP
  ens   <- option (Contract True []) ensures

  keyword "end"

  return $ AbsRoutine
             {
               routineFroz = fr
             , routineName = name
             , routineAlias  = als
             , routineArgs   = args
             , routineResult = res
             , routineAssigner = assgn
             , routineNote   = notes
             , routineProcs  = pGens
             , routineReq    = reqs
             , routineReqLk  = reqLk

             , routineImpl   = impl
             , routineEns    = ens
             , routineEnsLk  = ensLk
             }

assigner :: Parser String
assigner = do
  keyword "assign"
  identifier

alias = 
  let regStr = do  
        str <- stringTok
        if all (flip elem opSymbol) str || str == "[]"
          then return str
          else fail $ "unallowed alias symbol: " ++ str
      squareStr = do
        str <- blockStringTok
        if str == "" then return "[]" else fail $ "unallowed alias symbol: [" ++ str ++ "]"
  in do
    keyword "alias"
    regStr <|> squareStr

obsolete :: Parser String
obsolete = keyword "obsolete" >> stringTok

whichOf :: Parser a -> Parser a -> Parser Bool
whichOf p1 p2 = (p1 >> return True) <|> (p2 >> return False)

requires :: Parser (Contract Expr)
requires = do 
  inherited <- whichOf (keyword "require else") (keyword "require") 
  c <- many clause
  return $ Contract inherited c

ensures :: Parser (Contract Expr)
ensures = do 
  inherited <- whichOf (keyword "ensure then") (keyword "ensure") 
  c <- many clause
  return $ Contract inherited c

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


routineImplP = deferred <|> fullRoutineBody

deferred = do
  keyword "deferred"
  return RoutineDefer

fullRoutineBody :: Parser (RoutineBody Expr)
fullRoutineBody = do
  procs <- option [] (keyword "procs" >> many proc)
  decls <- concat `fmap` option [] (keyword "local" >> many decl)
  body  <- try external <|> featBody
  return (RoutineBody
          { routineLocal = decls
          , routineLocalProcs = procs
          , routineBody  = body
          }
         )

featBody :: Parser Stmt 
featBody = attachTokenPos $
           (keyword "do" <|> keyword "once") >> 
           Block `fmap` stmts
