{-# LANGUAGE KindSignatures #-}
module Language.Eiffel.Parser.Feature where

import Control.Applicative ((<$>), (<*>))

import Language.Eiffel.Syntax

import Language.Eiffel.Parser.Clause
import Language.Eiffel.Parser.Lex
import Language.Eiffel.Parser.Statement
import Language.Eiffel.Parser.Typ

import Text.Parsec

type FeatParser body exp = 
    Parser body -> Parser [AbsRoutine body exp]

data FeatureHead =
  FeatureHead 
  { fHeadNameAliases :: [NameAlias]
  , fHeadArgs :: [Decl]
  , fHeadRes :: Typ
  } deriving Show

data NameAlias = 
  NameAlias 
  { featureFrozen :: Bool
  , featureHeadName :: String
  , featureAlias :: Maybe String
  } deriving Show
    

nameAlias = do
  frz   <- (keyword TokFrozen >> return True) <|> return False
  name  <- identifier   <?> "Feature declaration identifier"
  als   <- optionMaybe alias
  return $ NameAlias frz name als

featureHead = do
  nameAls <- nameAlias `sepBy1` comma
  args    <- argumentList <?> "Argument list"
  res     <- option NoType (colon >> typ)
  optional (keyword TokIs)
  optional obsolete

  return (FeatureHead nameAls args res)

routine :: FeatureHead -> Maybe String -> [Note] -> Contract Expr
           -> FeatParser body Expr
routine fHead assgn notes reqs implP  = do
  let FeatureHead nameAls args res = fHead

  impl  <- implP
  ens   <- option (Contract True []) ensures
  rescue <- optionMaybe rescueP
  keyword TokEnd

  return $ map ( \ (NameAlias frz name als) ->
    AbsRoutine
     { routineFroz = frz
     , routineName = name
     , routineAlias  = als
     , routineArgs   = args
     , routineResult = res
     , routineAssigner = assgn
     , routineNote   = notes
     , routineReq    = reqs
     , routineImpl   = impl
     , routineEns    = ens
     , routineRescue = rescue
     , routineProcs  = []
     , routineReqLk  = []
     , routineEnsLk  = []
     }) nameAls

rescueP = do
  keyword TokRescue
  many stmt

assigner :: Parser String
assigner = do
  keyword TokAssign
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
        str <- stringTok -- FIXME: we don't lex block strings yet!, 
                         -- used to be: blockStringTok
        if str == "" then return "[]" else fail $ "unallowed alias symbol: [" ++ str ++ "]"
  in do
    keyword TokAlias
    regStr <|> squareStr

obsolete :: Parser String
obsolete = keyword TokObsolete >> stringTok

whichOf :: Parser a -> Parser a -> Parser Bool
whichOf p1 p2 = (p1 >> return True) <|> (p2 >> return False)

requires :: Parser (Contract Expr)
requires = do 
  inherited <- whichOf (keyword TokRequireElse) (keyword TokRequire) 
  c <- many clause
  return $ Contract inherited c

ensures :: Parser (Contract Expr)
ensures = do 
  inherited <- whichOf (keyword TokEnsureThen) (keyword TokEnsure) 
  c <- many clause
  return $ Contract inherited c

external :: Parser (RoutineBody exp)
external = RoutineExternal <$> (keyword TokExternal >> anyStringTok)
                           <*> optionMaybe (keyword TokAlias >> anyStringTok)

routineImplP = deferred <|> fullRoutineBody

deferred = do
  keyword TokDeferred
  return RoutineDefer

fullRoutineBody :: Parser (RoutineBody Expr)
fullRoutineBody = do
  decls <- concat `fmap` option [] (keyword TokLocal >> many decl)
  external <|> (do body <- featBody
                   return (RoutineBody
                             { routineLocal = decls
                             , routineBody  = body
                             , routineLocalProcs = []
                             }
                             ))

featBody :: Parser Stmt 
featBody = attachTokenPos $
           (keyword TokDo <|> keyword TokOnce) >> 
           Block `fmap` stmts
