{-# LANGUAGE KindSignatures #-}
module Language.Eiffel.Parser.Feature where

import Control.Applicative ((<$>), (<*>))

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Clause
import Language.Eiffel.Parser.Lex
import Language.Eiffel.Parser.Statement
import Language.Eiffel.Parser.Typ

import Text.Parsec

type FeatParser (body :: * -> *) exp = 
    Parser (body exp) -> Parser [AbsRoutine body exp]

data FeatureHead =
  FeatureHead 
  { fHeadNameAliases :: [NameAlias]
  , fHeadArgs :: [Decl]
  , fHeadRes :: Typ
  } deriving Show

data NameAlias = 
  NameAlias 
  { featureFrozen :: Bool
  , featureName :: String
  , featureAlias :: Maybe String
  } deriving Show
    

nameAlias = do
  frz   <- (keyword "frozen" >> return True) <|> return False
  name  <- identifier   <?> "Feature declaration identifier"
  als   <- optionMaybe alias
  return $ NameAlias frz name als

featureHead = do
  nameAls <- nameAlias `sepBy1` comma
  args    <- argumentList <?> "Argument list"
  res     <- option NoType (colon >> typ)
  optional (keyword "is")
  optional obsolete

  return (FeatureHead nameAls args res)

routine :: FeatureHead -> Maybe String -> [Note] -> Contract Expr
           -> FeatParser body Expr
routine fHead assgn notes reqs implP  = do
  let FeatureHead nameAls args res = fHead

  pGens <- option [] procGens

  reqLk <- option [] reqOrder
  ensLk <- option [] locks

  impl  <- implP
  ens   <- option (Contract True []) ensures
  rescue <- optionMaybe rescueP
  keyword "end"

  return $ map ( \ (NameAlias frz name als) ->
    AbsRoutine
     { routineFroz = frz
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
     , routineRescue = rescue
     }) nameAls

rescueP = do
  keyword "rescue"
  many stmt

assigner :: Parser String
assigner = do
  keyword "assign"
  identifier

allowedAliases = ["[]", "|..|", "and", "and then", "or", "or else", "implies",
                  "xor", "not"]

alias = 
  let regStr = do  
        str <- stringTok
        if all (flip elem opSymbol) str || str `elem` allowedAliases
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

external :: Parser (RoutineBody exp)
external = RoutineExternal <$> (keyword "external" >> anyStringTok)
                           <*> optionMaybe (keyword "alias" >> anyStringTok)

routineImplP = deferred <|> fullRoutineBody

deferred = do
  keyword "deferred"
  return RoutineDefer

fullRoutineBody :: Parser (RoutineBody Expr)
fullRoutineBody = do
  procs <- option [] (keyword "procs" >> many proc)
  decls <- concat `fmap` option [] (keyword "local" >> many decl)
  external <|> (do body <- featBody
                   return (RoutineBody
                             { routineLocal = decls
                             , routineLocalProcs = procs
                             , routineBody  = body
                             }
                             ))

featBody :: Parser Stmt 
featBody = attachTokenPos $
           (keyword "do" <|> keyword "once") >> 
           Block `fmap` stmts
