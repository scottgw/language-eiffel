{-# LANGUAGE FlexibleContexts #-}
module Language.Eiffel.Parser.Class where

import Control.Applicative ((<$>), (<*>), (<*))

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

inheritP :: Parser InheritClause
inheritP = do
  t <- classTyp
  (do 
      lookAhead (keyword "rename" <|> keyword "export" <|> keyword "undefine" <|> keyword "redefine" <|> keyword "select")
      renames <- option [] renameP
      exports <- option [] exportP
      undefs <- option [] undefineP
      redefs <- option [] redefineP
      selects <- option [] selectP
      keyword "end"
      return (InheritClause t renames exports undefs redefs selects)) <|> (return $ InheritClause t [] [] [] [] [])

renameP :: Parser [RenameClause]
renameP = do
  keyword "rename"
  renameName `sepBy` comma
  
renameName :: Parser RenameClause
renameName = do
  Rename <$> identifier 
         <*> (keyword "as" >> identifier)
         <*> optionMaybe alias
         
exportP :: Parser [ExportClause]
exportP = do 
  keyword "export"
  many (do 
    to <- braces (identifier `sepBy` comma)
    (do keyword "all"; return $ Export to ExportAll) <|> (do what <- identifier `sepBy` comma; return $ Export to (ExportFeatureNames what)))

undefineP = do
  keyword "undefine"
  identifier `sepBy` comma    
    
redefineP = do
  keyword "redefine"
  identifier `sepBy` comma
  
selectP = do
  keyword "select"
  identifier `sepBy` comma
  
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
absClas routineP = do
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
  fcs  <- absFeatureSects routineP
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
absFeatureSect routineP = do
  keyword "feature"
  exports <- option [] (braces (identifier `sepBy` comma))
  fds <- many (featureMember routineP)
  let (consts, featsAttrs) = partitionEithers fds
  let (feats, attrs) = partitionEithers featsAttrs
  return (FeatureClause exports feats attrs consts)


featureMember fp = do
  fHead <- featureHead
  
  let constant = case fHeadRes fHead of
        NoType -> fail "featureOrDecl: constant expects type"
        t -> Left <$> 
             Constant (fHeadFrozen fHead) (Decl (fHeadName fHead) t) <$> (opNamed "=" >> expr)
        
  let attrOrRoutine = do
        assign <- optionMaybe assigner
        notes <- option [] note
        reqs  <- option (Contract False []) requires

        let rout = routine fHead assign notes reqs fp
        case fHeadRes fHead of
          NoType -> Left <$> rout
          t -> (Left <$> rout) <|> (Right <$> (do
            ens <- if not (null notes) || not (null (contractClauses reqs))
                   then do
                     keyword "attribute"
                     ens <- option (Contract False []) ensures
                     keyword "end"
                     return ens
                   else return (Contract False [])
            let attr = Attribute (fHeadFrozen fHead) (Decl (fHeadName fHead) t) assign notes reqs ens
            return attr))
  
  constant <|> (Right <$> attrOrRoutine) <* optional semicolon

clas :: Parser Clas
clas = absClas routineImplP

clasInterfaceP :: Parser ClasInterface
clasInterfaceP = absClas (return EmptyBody)

