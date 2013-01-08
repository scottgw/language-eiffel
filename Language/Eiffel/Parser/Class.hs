{-# LANGUAGE FlexibleContexts #-}
module Language.Eiffel.Parser.Class where

import           Control.Applicative ((<$>), (<*>), (<*), (*>))

import           Data.Char
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)

import           Language.Eiffel.Syntax
import           Language.Eiffel.Util

import           Language.Eiffel.Parser.Lex
import           Language.Eiffel.Parser.Clause
import           Language.Eiffel.Parser.Expr
import           Language.Eiffel.Parser.Feature
import           Language.Eiffel.Parser.Note
import           Language.Eiffel.Parser.Typ

import           Text.Parsec

genericsP :: Parser [Generic]
genericsP = squares (sepBy genericP comma)

genericP :: Parser Generic
genericP = do
  name <- identifier
  typs <- option [] (do opNamed "->" 
                        braces (typ `sepBy1` comma) <|> fmap (replicate 1) typ)
  creations <- optionMaybe 
              (keyword TokCreate *> (identifier `sepBy1` comma) <* keyword TokEnd)
  return (Generic name typs creations)

invariants :: Parser [Clause Expr]
invariants = keyword TokInvariant >> many clause

inherits :: Parser [Inheritance]
inherits = many inheritP

inheritP = do
  keyword TokInherit
  nonConf <- (braces identifier >> return True) <|> return False
  inClauses <- many inheritClauseP
  return (Inheritance nonConf inClauses)

inheritClauseP :: Parser InheritClause
inheritClauseP = do
  t <- classTyp
  (do 
      lookAhead (keyword TokRename <|> keyword TokExport <|> keyword TokUndefine <|> keyword TokRedefine <|> keyword TokSelect)
      renames <- option [] renameP
      exports <- option [] exportP
      undefs <- option [] undefineP
      redefs <- option [] redefineP
      selects <- option [] selectP
      keyword TokEnd
      return (InheritClause t renames exports undefs redefs selects)) <|> (return $ InheritClause t [] [] [] [] [])

renameP :: Parser [RenameClause]
renameP = do
  keyword TokRename
  renameName `sepBy` comma
  
renameName :: Parser RenameClause
renameName = do
  Rename <$> identifier 
         <*> (keyword TokAs >> identifier)
         <*> optionMaybe alias
         
exportP :: Parser [ExportClause]
exportP = do 
  keyword TokExport
  many (do 
    to <- braces (identifier `sepBy` comma)
    (do keyword TokAll; return $ Export to ExportAll) <|> (do what <- identifier `sepBy` comma; return $ Export to (ExportFeatureNames what)))

undefineP = do
  keyword TokUndefine
  identifier `sepBy` comma    
    
redefineP = do
  keyword TokRedefine
  identifier `sepBy` comma
  
selectP = do
  keyword TokSelect
  identifier `sepBy` comma
  
create :: Parser CreateClause
create = do
  keyword TokCreate
  exports <- option [] (braces (identifier `sepBy` comma))
  names <- identifier `sepBy` comma
  return (CreateClause exports names)
  
convertsP :: Parser [ConvertClause]
convertsP = do
  keyword TokConvert
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
  frz   <- option False (keyword TokFrozen >> return True)
  expand <- option False (keyword TokExpanded >> return True)
  def   <- option False (keyword TokDeferred >> return True)
  keyword TokClass
  name <- identifier
  gen  <- option [] genericsP
  obs  <- option False (keyword TokObsolete >> 
                        option True (anyStringTok >> return True))
  is   <- option [] inherits
  cs   <- many create
  cnvs <- option [] convertsP
  fcs  <- absFeatureSects routineP
  invs <- option [] invariants
  endNotes <- option [] note
  keyword TokEnd
  return ( AbsClas 
           { frozenClass = frz
           , expandedClass = expand     
           , deferredClass = def
           , classNote  = notes ++ endNotes
           , className  = name
           , currProc   = Dot
           , generics   = gen 
           , obsoleteClass = obs
           , inherit    = is
           , creates    = cs
           , converts   = cnvs
           , featureMap = fcs
           , invnts     = invs
           , procGeneric = []
           , procExpr = []
           }
         )

absFeatureSects :: Parser body 
                   -> Parser (FeatureMap body Expr)
absFeatureSects bodyP = fmUnions <$> many (absFeatureSect bodyP)

absFeatureSect :: Parser body 
                  -> Parser (FeatureMap body Expr)
absFeatureSect routineP = do
  keyword TokFeature
  exports <- Set.fromList <$> option [] (braces (identifier `sepBy` comma))
  fmUnions <$> many (featureMember exports routineP)

constWithHead fHead t = 
  let mkConst (NameAlias frz name _als) = Constant frz (Decl name t)
      constStarts = map mkConst (fHeadNameAliases fHead)
  in do
    e <- opInfo (RelOp Eq NoType) >> expr
    optional semicolon
    return (map ($ e) constStarts)

attrWithHead fHead assign notes reqs t = do
  ens <- if not (null notes) || not (null (contractClauses reqs))
         then do
           keyword TokAttribute
           ens <- option (Contract True []) ensures
           keyword TokEnd
           return ens
         else optional (keyword TokAttribute >> keyword TokEnd) >>
              return (Contract False [])
  let mkAttr (NameAlias frz name _als) = 
        Attribute frz (Decl name t) assign notes reqs ens
  return (map mkAttr (fHeadNameAliases fHead))

featureMember :: Set String -> Parser body -> Parser (FeatureMap body Expr)
featureMember exports fp = do
  fHead <- featureHead
  
  let
    mkMap :: Feature f Expr 
             => [f]
             -> Map String (ExportedFeature f)
    mkMap = 
      Map.fromList . 
      map (\f -> (map toLower (featureName f), ExportedFeature exports f))
    
    mkRoutMap x = FeatureMap x Map.empty Map.empty
    mkAttrMap x = FeatureMap Map.empty x Map.empty
    mkConstMap x = FeatureMap Map.empty Map.empty x
    
    constant = case fHeadRes fHead of
      NoType -> fail "featureOrDecl: constant expects type"
      t -> mkConstMap <$> mkMap <$> constWithHead fHead t 
      
    attrOrRoutine = do
      assign <- optionMaybe assigner
      notes <- option [] note
      reqs  <- option (Contract True []) requires

      let 
        rout = routine fHead assign notes reqs fp
        someRout = mkRoutMap <$> mkMap <$> rout
      case fHeadRes fHead of
        NoType -> someRout
        t -> someRout <|> 
             (mkAttrMap <$> mkMap <$> attrWithHead fHead assign notes reqs t)
  constant <|> attrOrRoutine <* optional semicolon

clas :: Parser Clas
clas = absClas routineImplP

clasInterfaceP :: Parser ClasInterface
clasInterfaceP =  absClas (keyword TokDo >> return EmptyBody)
           


