{-# LANGUAGE FlexibleContexts #-}
module Language.Eiffel.Parser.Class where

import Control.Applicative ((<$>), (<*>))
import Control.Monad

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
  identifier `sepBy` comma

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
  option [] (braces (identifier `sepBy` comma)) -- ToDo: creation export is thrown away
  identifier `sepBy` comma
  
convertsP :: Parser [()]
convertsP = do
  keyword "convert"
  convert `sepBy` comma

convert :: Parser ()
convert = identifier >>
  (do { colon; braces typ } <|> parens (braces typ)) >>
  return () -- ToDo: convert clauses are thrown away

absClas :: Parser (body Expr) -> Parser (AbsClas body Expr)
absClas featureP = do
  notes <- option [] note
  def   <- option False (keyword "deferred" >> return True)
  keyword "class"
  name <- identifier
  gen  <- option [] genericsP
  pgs  <- option [] procGens
  pes  <- many proc
  is   <- option [] inherits
  cs   <- option [] createsP
  cnvs <- option [] convertsP
  fcs  <- absFeatureSects featureP
  invs <- option [] invariants
  endNotes <- option [] note
  keyword "end" 
  return ( AbsClas 
           { deferredClass = def
           , classNote  = notes ++ endNotes
           , className  = name
           , currProc   = Dot
           , procGeneric = pgs
           , procExpr   = pes
           , generics   = gen 
           , inherit    = is
           , creates    = cs
           , featureClauses = fcs
           , invnts     = invs
           }
         )

absFeatureSects :: Parser (body Expr) -> Parser [FeatureClause body Expr]
absFeatureSects = many . absFeatureSect

absFeatureSect :: Parser (body Expr) -> Parser (FeatureClause body Expr)
absFeatureSect featureP = do
  keyword "feature"
  export <- option [] (braces (identifier `sepBy` comma))
  fds <- many (featureMember featureP) -- absFeatureOrDecls featureP
  let (consts, featsAttrs) = partitionEithers fds
  let (feats, attrs) = partitionEithers featsAttrs
  return (FeatureClause export feats attrs consts)


featureMember fp = do
  fHead <- featureHead
  
  let constant = case fHeadRes fHead of
        NoType -> fail "featureOrDecl: constant expects type"
        t -> Left <$> 
             Constant (Decl (fHeadName fHead) t) <$> (opNamed "=" >> expr)
        
  let attrOrRoutine = do
        assign <- optionMaybe assigner
        notes <- option [] note
        let routine = feature fHead assign notes fp        
        case fHeadRes fHead of
          NoType -> Left <$> routine
          t -> (Left <$> routine) <|> (Right <$> (do
            let attr = Attribute (Decl (fHeadName fHead) t) assign notes
            when (not $ null notes) (keyword "attribute" >> keyword "end")
            return attr))
  
  constant <|> (Right <$> attrOrRoutine)


clas :: Parser Clas
clas = absClas featureImplP

clasInterfaceP :: Parser ClasInterface
clasInterfaceP = absClas (return EmptyBody)

