{-# LANGUAGE FlexibleContexts #-}
module Language.Eiffel.Parser.Class where

import Control.Applicative ((<$>), (<*>))

import Language.Eiffel.Eiffel

import Language.Eiffel.Parser.Lex
import Language.Eiffel.Parser.Clause
import Language.Eiffel.Parser.Expr
import Language.Eiffel.Parser.Feature
import Language.Eiffel.Parser.Note
import Language.Eiffel.Parser.Typ

import Data.Either

import Text.Parsec

genericsP :: Parser [Generic]
genericsP = squares (sepBy genericP comma)

genericP :: Parser Generic
genericP = Generic `fmap` identifier

invariants :: Parser [Clause Expr]
invariants = keyword "invariant" >> many clause

inherits :: Parser [InheritClause]
inherits = keyword "inherit" >> many inheritP

redefineP = do
  keyword "redefine"
  many identifier

inheritP :: Parser InheritClause
inheritP = do
  t <- classTyp
  (do 
      lookAhead (keyword "rename" <|> keyword "redefine")
      renames <- option [] rename
      redefs <- option [] redefineP
      keyword "end"
      return (InheritClause t redefs renames)) <|> (return $ InheritClause t [] [])

rename :: Parser [RenameClause]
rename = do
  keyword "rename"
  renameName `sepBy` comma
  
renameName :: Parser RenameClause
renameName = do
  Rename <$> identifier 
         <*> (keyword "as" >> identifier)
         <*> optionMaybe alias
  
createsP :: Parser [String]
createsP = do
  keyword "create"
  many identifier

absClas :: Parser (body Expr) -> Parser (AbsClas body Expr)
absClas featureP = do
  notes <- option [] note
  optional (keyword "deferred")
  keyword "class"
  name <- identifier
  gen  <- option [] genericsP
  pgs  <- option [] procGens
  pes  <- many proc
  is   <- option [] inherits
  cs   <- option [] createsP
  (fs, ds) <- absFeatureSects featureP
  invs <- option [] invariants
  keyword "end" 
  return ( AbsClas 
           { classNote  = notes
           , className  = name
           , currProc   = Dot
           , procGeneric = pgs
           , procExpr   = pes
           , generics   = gen 
           , inherit    = is
           , creates    = cs
           , attributes = ds
           , features   = fs
           , invnts     = invs
           }
         )

absFeatureSects :: Parser (body Expr) -> Parser ([AbsFeature body Expr], [Decl])
absFeatureSects = fmap (foldl f ([],[])) . many . absFeatureSect
    where f (fs, ds) (fs', ds') = (fs ++ fs', ds ++ concat ds')

absFeatureSect :: Parser (body Expr) -> Parser ([AbsFeature body Expr], [[Decl]])
absFeatureSect featureP = do
  keyword "feature"
  optional (braces identifier)
  fds <- absFeatureOrDecls featureP
  return $ partitionEithers fds

absFeatureOrDecls :: Parser (body Expr) 
                  -> Parser [Either (AbsFeature body Expr) [Decl]]
absFeatureOrDecls = many .  absFeatureOrDecl

absFeatureOrDecl :: Parser (body Expr) 
                 -> Parser (Either (AbsFeature body Expr) [Decl])
absFeatureOrDecl fp = try onlyDecl <|> absArgFeature fp
-- the order of the above matters, it would be nice to eliminate that

onlyDecl :: Parser (Either a [Decl])
onlyDecl = do
  d <- declEq
  functionIndicators
  return (Right d)


declEq :: Parser [Decl]
declEq = do
  d <- decl
  optional (opNamed "=" >> expr)
  return d


resrv :: String -> Parser Char
resrv str = keyword str >> return 'a'

functionIndicators :: Parser ()
functionIndicators = do
  notFollowedBy (choice (map keyword ["do", "external", "once", "is", "deferred", "local"
                                     ,"procs", "require", "require-locks"] ))
  -- key <- lookAhead someKeyword
  -- if key `elem` ["do", "external", "once", "is", "deferred", "local"
  --               ,"procs", "require", "require-locks"] 
  --   then parserZero
  --   else return ()

absArgFeature :: Parser (body Expr) 
              -> Parser (Either (AbsFeature body Expr) [Decl])
absArgFeature = fmap Left . feature

clas :: Parser Clas
clas = absClas featureImplP

clasInterfaceP :: Parser ClasInterface
clasInterfaceP = absClas (return EmptyBody)