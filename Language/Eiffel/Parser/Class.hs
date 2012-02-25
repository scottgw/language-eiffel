{-# LANGUAGE FlexibleContexts #-}
module Language.Eiffel.Parser.Class where

import Control.Applicative ((<$>), (<*>), (<*))
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
genericP = do
  name <- identifier
  constr <- option [] (opNamed "->" >> (braces (typ `sepBy1` comma) <|> (fmap (replicate 1) typ)))
  return (Generic name constr)

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
  
create :: Parser CreateClause
create = do
  keyword "create"
  exports <- option [] (braces (identifier `sepBy` comma))
  names <- identifier `sepBy` comma
  return (CreateClause exports names)
  
convertsP :: Parser [ConvertClause]
convertsP = do
  keyword "convert"
  convert `sepBy` comma

convert :: Parser ConvertClause
convert = do
  fname <- identifier
  (do 
    colon
    t <- braces typ
    return (ConvertTo fname t)) <|> (do
    t <- parens (braces typ)
    return (ConvertFrom fname t))

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
  cs   <- many create
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
           , converts   = cnvs
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
        reqs  <- option [] requires

        let routine = feature fHead assign notes reqs fp
        case fHeadRes fHead of
          NoType -> Left <$> routine
          t -> (Left <$> routine) <|> (Right <$> (do
            ens <- if not (null notes) || not (null reqs)
                   then do
                     keyword "attribute"
                     ens <- option [] (keyword "ensure" >> many1 clause)
                     keyword "end"
                     return ens
                   else return []
            let attr = Attribute (Decl (fHeadName fHead) t) assign notes reqs ens
            return attr))
  
  constant <|> (Right <$> attrOrRoutine) <* optional semicolon

clas :: Parser Clas
clas = absClas featureImplP

clasInterfaceP :: Parser ClasInterface
clasInterfaceP = absClas (return EmptyBody)

