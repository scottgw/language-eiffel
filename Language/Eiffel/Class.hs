{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Eiffel.Class where

import qualified Data.Map as Map
import Data.Map (Map) 
import Data.Maybe (listToMaybe)

import Language.Eiffel.Decl
import Language.Eiffel.Clause
import Language.Eiffel.Expr
import Language.Eiffel.Feature
import Language.Eiffel.Note
import Language.Eiffel.Typ

type Clas = ClasBody Expr
type ClasBody exp = AbsClas RoutineBody exp
type ClasInterface = AbsClas EmptyBody Expr
type ClasI exp = AbsClas RoutineBody exp

data AbsClas (body :: * -> *) exp =
    AbsClas
    {
      deferredClass :: Bool,
      classNote  :: [Note],
      className  :: ClassName,
      currProc   :: Proc,
      procGeneric :: [Proc],
      procExpr   :: [ProcDecl],
      generics   :: [Generic],
      inherit    :: [InheritClause],
      creates    :: [CreateClause],
      converts   :: [ConvertClause],
      featureClauses   :: [FeatureClause body exp],
      invnts     :: [Clause exp]
    } deriving (Eq, Show)

data InheritClause 
    = InheritClause 
      { inheritClass :: Typ
      , rename :: [RenameClause]
      , export :: [ExportClause]
      , undefine :: [String]
      , redefine :: [String]
      , select :: [String]
      } deriving (Show, Eq)
                 
data RenameClause = 
  Rename { renameOrig :: String
         , renameNew :: String
         , renameAlias :: Maybe String
         } deriving (Show, Eq)

data ExportList = ExportFeatureNames [String] | ExportAll deriving (Show, Eq)
         
data ExportClause = 
  Export { exportTo :: [ClassName]
         , exportWhat :: ExportList
         } deriving (Show, Eq)

data Generic = 
  Generic { genericName :: ClassName 
         , genericConstraints :: [Typ]
         } deriving (Show, Eq) 

data CreateClause = 
  CreateClause { createExportNames :: [ClassName]
               , createNames :: [String]
               } deriving (Show, Eq)
		 
data ConvertClause = ConvertFrom String Typ 
                   | ConvertTo String Typ deriving (Show, Eq)

data FeatureClause body exp =
  FeatureClause { exportNames :: [ClassName]
                , routines :: [AbsRoutine body exp]
                , attributes :: [Attribute exp]
                , constants :: [Constant exp]
                } deriving (Show, Eq)


class Feature a => ClassFeature a body expr | a -> expr, a -> body where
  allFeatures :: AbsClas body expr -> [a]
  
instance ClassFeature (Constant expr) body expr where
  allFeatures = allConstants
  
instance ClassFeature (AbsRoutine body expr) body expr where
  allFeatures = allRoutines

instance ClassFeature (Attribute expr) body expr where
  allFeatures = allAttributes

instance ClassFeature FeatureEx body expr where
  allFeatures clas = map FeatureEx (allAttributes clas) ++
                     map FeatureEx (allRoutines clas) ++
                     map FeatureEx (allConstants clas)

constToAttr :: Constant exp -> Attribute Expr
constToAttr (Constant froz d _) = 
  Attribute froz d Nothing [] (Contract False []) (Contract False [])

allAttributes = concatMap attributes . featureClauses
allRoutines = concatMap routines . featureClauses
allConstants = concatMap constants . featureClauses
allCreates = concatMap createNames . creates
allAttributeDecls = map attrDecl . allAttributes
allConstantDecls = map constDecl . allConstants

isCreateName n c = n `elem` allCreates c

mapRoutines f clause = clause {routines = map f (routines clause)}
mapAttributes f clause = clause {attributes = map f (attributes clause)}
mapConstants f clause = clause {constants = map f (constants clause)}

mapContract clauseF cs =
  cs { contractClauses = map clauseF (contractClauses cs)}

mapExprs featrF constF clauseF fClause = 
  fClause { routines = map featrF (routines fClause)
          , constants = map constF (constants fClause)
          , attributes = 
               map (\a -> a { attrEns = mapContract clauseF (attrEns a)
                            , attrReq = mapContract clauseF (attrReq a)
                            }
                   ) (attributes fClause)
         }

classMapAttributes f c = 
  c {featureClauses = map (mapAttributes f) (featureClauses c)}

classMapRoutines :: (AbsRoutine body exp -> AbsRoutine body exp) 
            -> AbsClas body exp -> AbsClas body exp
classMapRoutines f c = 
  c {featureClauses = map (mapRoutines f) (featureClauses c)}



classMapExprs :: (AbsRoutine body exp -> AbsRoutine body' exp') 
                 -> (Clause exp -> Clause exp')
                 -> (Constant exp -> Constant exp')
                 -> AbsClas body exp -> AbsClas body' exp'
classMapExprs featrF clauseF constF c = 
  c { featureClauses = map (mapExprs featrF constF clauseF) (featureClauses c)
    , invnts         = map clauseF (invnts c)
    }


makeRoutineIs :: FeatureClause body exp -> FeatureClause EmptyBody Expr
makeRoutineIs clause =
  clause { routines   = map makeRoutineI (routines clause)
         , attributes = map makeAttributeI (attributes clause) ++ 
                        map constToAttr (constants clause)
         , constants  = []
         }

makeAttributeI :: Attribute exp -> Attribute Expr
makeAttributeI (Attribute froz decl assgn notes _ _) =
  Attribute froz decl assgn notes (Contract False []) (Contract False [])

clasInterface :: AbsClas body exp -> ClasInterface
clasInterface c = 
  c { featureClauses = map makeRoutineIs (featureClauses c)
    , invnts = []}

clasMap :: [AbsClas body exp] -> Map ClassName (AbsClas body exp)
clasMap = Map.fromList . map (\ c -> (className c, c))

attrMap :: AbsClas body exp -> Map String Typ
attrMap = declsToMap . map attrDecl . allAttributes

findRoutine :: Clas -> String -> Maybe Routine
findRoutine = findFeature

findOperator :: ClasInterface -> String -> Maybe RoutineI
findOperator c opName =
    let fs = allRoutines c
        ffs = filter ( (== Just opName) . routineAlias) fs
    in listToMaybe ffs

findFeature :: ClassFeature a body expr => 
               AbsClas body expr -> String -> Maybe a
findFeature clasInt name = 
  let fs = filter ( (== name) . featureName) (allFeatures clasInt)
  in listToMaybe fs

findFeatureEx :: AbsClas body expr -> String -> Maybe FeatureEx
findFeatureEx = findFeature

findRoutineInt :: ClasInterface -> String -> Maybe RoutineI
findRoutineInt = findFeature

findAttrInt :: ClasInterface -> String -> Maybe (Attribute Expr)
findAttrInt = findFeature    

findConstantInt :: ClasInterface -> String -> Maybe (Constant Expr)
findConstantInt = findFeature 

updFeatureClauses :: AbsClas body exp -> [FeatureClause body exp] 
                     -> AbsClas body exp
updFeatureClauses c fcs = c {featureClauses = fcs}

fullName :: AbsClas body exp -> RoutineI -> String
fullName c f = fullNameStr (className c) (routineName f)

fullNameStr :: String -> String -> String
fullNameStr = (++)

genericStubs :: AbsClas body exp -> [AbsClas body exp]
genericStubs = map makeGenericStub . generics

-- for the G,H in something like `class A [G,H]'
makeGenericStub :: Generic -> AbsClas body exp
makeGenericStub (Generic g _) = AbsClas 
                  { deferredClass = False
                  , classNote  = []
                  , className  = g
                  , currProc   = Dot
                  , procGeneric = []
                  , procExpr   = []
                  , generics   = []
                  , inherit    = []
                  , creates    = []
                  , converts   = []
                  , featureClauses   = []
                  , invnts     = []
                  }
