{-# LANGUAGE FlexibleContexts #-}
module Language.Eiffel.Parser.Class where

import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Language.Eiffel.Syntax

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
  typs <- option [] (do opNamed "->" 
                        braces (typ `sepBy1` comma) <|> fmap (replicate 1) typ)
  creations <- optionMaybe 
              (keyword "create" *> (identifier `sepBy1` comma) <* keyword "end")
  return (Generic name typs creations)

invariants :: Parser [Clause Expr]
invariants = keyword "invariant" >> many clause

inherits :: Parser [Inheritance]
inherits = many inheritP

inheritP = do
  keyword "inherit"
  nonConf <- (braces identifier >> return True) <|> return False
  inClauses <- many inheritClauseP
  return (Inheritance nonConf inClauses)

inheritClauseP :: Parser InheritClause
inheritClauseP = do
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
    ts <- braces (typ `sepBy1` comma)
    return (ConvertTo fname ts)) <|> (do
    ts <- parens (braces (typ `sepBy1` comma))
    return (ConvertFrom fname ts))

absClas :: Parser body -> Parser (AbsClas body Expr)
absClas routineP = do
  notes <- option [] note
  frz   <- option False (keyword "frozen" >> return True)
  expand <- option False (keyword "expanded" >> return True)
  def   <- option False (keyword "deferred" >> return True)
  keyword "class"
  name <- identifier
  gen  <- option [] genericsP
  pgs  <- option [] procGens
  pes  <- many proc
  obs  <- option False (keyword "obsolete" >> 
                        option True (anyStringTok >> return True))
  is   <- option [] inherits
  cs   <- many create
  cnvs <- option [] convertsP
  fcs  <- absFeatureSects routineP
  invs <- option [] invariants
  endNotes <- option [] note
  keyword "end" 
  return ( AbsClas 
           { frozenClass = frz
           , expandedClass = expand     
           , deferredClass = def
           , classNote  = notes ++ endNotes
           , className  = name
           , currProc   = Dot
           , procGeneric = pgs
           , procExpr   = pes
           , generics   = gen 
           , obsoleteClass = obs
           , inherit    = is
           , creates    = cs
           , converts   = cnvs
           , featureClauses = fcs
           , invnts     = invs
           }
         )

absFeatureSects :: Parser body -> Parser [FeatureClause body Expr]
absFeatureSects = many . absFeatureSect

absFeatureSect :: Parser body -> Parser (FeatureClause body Expr)
absFeatureSect routineP = do
  keyword "feature"
  exports <- option [] (braces (identifier `sepBy` comma))
  fds <- many (featureMember routineP)
  let (consts, featsAttrs) = partitionEithers fds
  let (feats, attrs) = partitionEithers featsAttrs
  return (FeatureClause exports (concat feats) (concat attrs) (concat consts))


constWithHead fHead t = 
  let mkConst (NameAlias frz name _als) = Constant frz (Decl name t)
      constStarts = map mkConst (fHeadNameAliases fHead)
  in do
    e <- opNamed "=" >> expr
    optional semicolon
    return  (map ($ e) constStarts)

attrWithHead fHead assign notes reqs t = do
  ens <- if not (null notes) || not (null (contractClauses reqs))
         then do
           keyword "attribute"
           ens <- option (Contract True []) ensures
           keyword "end"
           return ens
         else optional (keyword "attribute" >> keyword "end") >>
              return (Contract False [])
  let mkAttr (NameAlias frz name _als) = 
        Attribute frz (Decl name t) assign notes reqs ens
  return (map mkAttr (fHeadNameAliases fHead))

featureMember fp = do
  fHead <- featureHead
  
  let constant = case fHeadRes fHead of
        NoType -> fail "featureOrDecl: constant expects type"
        t -> Left <$> constWithHead fHead t 
        
  let attrOrRoutine = do
        assign <- optionMaybe assigner
        notes <- option [] note
        reqs  <- option (Contract True []) requires

        let rout = routine fHead assign notes reqs fp
        case fHeadRes fHead of
          NoType -> Left <$> rout
          t -> (Left <$> rout) <|> 
               (Right <$> attrWithHead fHead assign notes reqs t)
  constant <|> (Right <$> attrOrRoutine) <* optional semicolon

clas :: Parser Clas
clas = absClas routineImplP

clasInterfaceP :: Parser ClasInterface
clasInterfaceP =  absClas (keyword "do" >> return EmptyBody)
           


