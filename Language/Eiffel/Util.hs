{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Eiffel.Util where

import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)

import Language.Eiffel.Syntax

-- Class level utilities

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




class Feature a where
  featureName     :: a -> String
  featureArgs     :: a -> [Decl]
  featureResult   :: a -> Typ
  featureIsFrozen :: a -> Bool

data FeatureEx = forall a. Feature a => FeatureEx a

instance Feature FeatureEx where
  featureName (FeatureEx f) = featureName f
  featureArgs (FeatureEx f) = featureArgs f
  featureResult (FeatureEx f) = featureResult f
  featureIsFrozen (FeatureEx f) = featureIsFrozen f

instance Feature (AbsRoutine body exp) where
  featureName = routineName
  featureArgs = routineArgs
  featureResult = routineResult
  featureIsFrozen = routineFroz

instance Feature (Attribute exp) where
  featureName = declName . attrDecl
  featureArgs = const []
  featureResult = declType . attrDecl
  featureIsFrozen = attrFroz

instance Feature (Constant exp) where
  featureName = declName . constDecl
  featureArgs = const []
  featureResult = declType . constDecl
  featureIsFrozen = constFroz
 


constToAttr :: Constant exp -> Attribute Expr
constToAttr (Constant froz d _) = 
  Attribute froz d Nothing [] (Contract False []) (Contract False [])

allAttributes = concatMap attributes . featureClauses
allRoutines = concatMap routines . featureClauses
allConstants = concatMap constants . featureClauses
allCreates = concatMap createNames . creates
allAttributeDecls = map attrDecl . allAttributes
allConstantDecls = map constDecl . allConstants
allInheritedTypes = concatMap (map inheritClass . inheritClauses) . inherit
isCreateName n c = n `elem` allCreates c

mapRoutines f clause = clause {routines = map f (routines clause)}
mapRoutinesM :: Monad m =>
                (AbsRoutine body exp -> m (AbsRoutine body exp)) ->
                FeatureClause body exp -> 
                m (FeatureClause body exp)
mapRoutinesM f clause = do
  routs <- mapM f (routines clause)
  return (clause {routines = routs})


mapAttributes f clause = clause {attributes = map f (attributes clause)}

mapAttributesM :: Monad m =>
                  (Attribute exp -> m (Attribute exp)) ->
                  FeatureClause body exp -> 
                  m (FeatureClause body exp)
mapAttributesM f clause = do
  attrs <- mapM f (attributes clause)
  return (clause {attributes = attrs})

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

classMapAttributesM :: Monad m =>
                       (Attribute exp -> m (Attribute exp)) ->
                       AbsClas body exp -> 
                       m (AbsClas body exp)
classMapAttributesM f c = do
  cs <- mapM (mapAttributesM f) (featureClauses c)
  return (c {featureClauses = cs})


classMapRoutines :: (AbsRoutine body exp -> AbsRoutine body exp) 
                    -> AbsClas body exp -> AbsClas body exp
classMapRoutines f c = 
  c {featureClauses = map (mapRoutines f) (featureClauses c)}
classMapRoutinesM :: Monad m =>
                       (AbsRoutine body exp -> m (AbsRoutine body exp)) ->
                       AbsClas body exp -> 
                       m (AbsClas body exp)
classMapRoutinesM f c = do
  cs <- mapM (mapRoutinesM f) (featureClauses c)
  return (c {featureClauses = cs})




classMapExprs :: (AbsRoutine body exp -> AbsRoutine body' exp') 
                 -> (Clause exp -> Clause exp')
                 -> (Constant exp -> Constant exp')
                 -> AbsClas body exp -> AbsClas body' exp'
classMapExprs featrF clauseF constF c = 
  c { featureClauses = map (mapExprs featrF constF clauseF) (featureClauses c)
    , invnts         = map clauseF (invnts c)
    }


makeRoutineIs :: FeatureClause body Expr -> FeatureClause EmptyBody Expr
makeRoutineIs clause =
  clause { routines = map makeRoutineI (routines clause) }

makeAttributeI :: Attribute exp -> Attribute Expr
makeAttributeI (Attribute froz decl assgn notes _ _) =
  Attribute froz decl assgn notes (Contract False []) (Contract False [])

clasInterface :: AbsClas body Expr -> ClasInterface
clasInterface c = 
  c { featureClauses = map makeRoutineIs (featureClauses c) }

clasMap :: [AbsClas body exp] -> Map ClassName (AbsClas body exp)
clasMap = Map.fromList . map (\ c -> (className c, c))

attrMap :: AbsClas body exp -> Map String Typ
attrMap = declsToMap . map attrDecl . allAttributes

findRoutine :: Clas -> String -> Maybe Routine
findRoutine = findFeature

findOperator :: AbsClas body Expr -> String -> Maybe (AbsRoutine body Expr)
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

findAttrInt :: AbsClas body Expr -> String -> Maybe (Attribute Expr)
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
makeGenericStub (Generic g _ _) = AbsClas 
                  { deferredClass = False
                  , frozenClass = False
                  , expandedClass = False
                  , classNote  = []
                  , className  = g
                  , currProc   = Dot
                  , procGeneric = []
                  , obsoleteClass = False
                  , procExpr   = []
                  , generics   = []
                  , inherit    = []
                  , creates    = []
                  , converts   = []
                  , featureClauses   = []
                  , invnts     = []
                  }



-- Routine level utilities
makeRoutineI :: AbsRoutine body Expr -> RoutineI
makeRoutineI f = f { routineImpl = EmptyBody 
                   , routineRescue = Nothing}

argMap :: RoutineWithBody a -> Map String Typ
argMap = declsToMap . routineArgs

localMap :: RoutineWithBody a -> Map String Typ
localMap = declsToMap . routineDecls

routineDecls r =
  case routineImpl r of
    RoutineDefer -> []
    RoutineExternal _ _ -> []
    body -> routineLocal body


updFeatBody :: RoutineBody a -> PosAbsStmt b -> RoutineBody b
updFeatBody impl body = impl {routineBody = body}

-- Type utilities

isBasic :: Typ -> Bool
isBasic (ClassType name _) = name `elem` basicNames
  where basicNames = concat [ map (("INTEGER_" ++) . show) [16, 32, 64]
                            , map (("NATURAL_" ++) . show) [8, 16, 32, 64]
                            , ["REAL_32", "REAL_64"]
                            , ["CHARACTER_8", "CHARACTER_32"]
                            ]
isBasic _          = False


classNameType :: Typ -> String
classNameType (ClassType cn _) = cn 
classNameType (Sep _ _ cn) = cn
classNameType _ = error "Non-class type"

-- Decl utilities
insertDecl :: Decl -> Map String Typ -> Map String Typ
insertDecl (Decl s t) = Map.insert s t

declsToMap :: [Decl] -> Map String Typ
declsToMap = foldr insertDecl Map.empty

-- SCOOP utilities

newVar :: ProcDecl -> Proc
newVar (SubTop   p) = p
newVar (CreateLessThan p _) = p
