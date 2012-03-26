{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Eiffel.Util where

import Data.Char
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
  featureRename   :: a -> RenameClause -> a

data FeatureEx = forall a. Feature a => FeatureEx a

instance Feature FeatureEx where
  featureName (FeatureEx f) = featureName f
  featureArgs (FeatureEx f) = featureArgs f
  featureResult (FeatureEx f) = featureResult f
  featureIsFrozen (FeatureEx f) = featureIsFrozen f
  featureRename (FeatureEx f) = FeatureEx . featureRename f

instance Feature (AbsRoutine body exp) where
  featureName = routineName
  featureArgs = routineArgs
  featureResult = routineResult
  featureIsFrozen = routineFroz
  featureRename rout r@(Rename orig new alias)
    | routineName rout == orig = rout { routineName = new
                                      , routineAlias = alias
                                      , routineArgs = newArgs
                                      } 
    | otherwise = rout {routineArgs = newArgs}
    where newArgs = map (renameDecl r) (routineArgs rout)

instance Feature (Attribute exp) where
  featureName = declName . attrDecl
  featureArgs = const []
  featureResult = declType . attrDecl
  featureIsFrozen = attrFroz
  featureRename attr r =
    attr {attrDecl = renameDecl r (attrDecl attr)}

instance Feature (Constant exp) where
  featureName = declName . constDecl
  featureArgs = const []
  featureResult = declType . constDecl
  featureIsFrozen = constFroz
  featureRename constant r =
    constant {constDecl = renameDecl r (constDecl constant)}

     


constToAttr :: Constant exp -> Attribute Expr
constToAttr (Constant froz d _) = 
  Attribute froz d Nothing [] (Contract False []) (Contract False [])

allAttributes = concatMap attributes . featureClauses
allRoutines = concatMap routines . featureClauses
allConstants = concatMap constants . featureClauses
allCreates = concatMap createNames . creates
allAttributeDecls = map attrDecl . allAttributes
allConstantDecls = map constDecl . allConstants
allInherited = concatMap inheritClauses . inherit
allInheritedTypes = map inheritClass . allInherited
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


classMapConstants f c =
  c {featureClauses = map (mapConstants f) (featureClauses c)}

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

findOperator :: AbsClas body Expr -> String -> Int -> 
                Maybe (AbsRoutine body Expr)
findOperator c opName numArgs =
    let fs = allRoutines c
        ffs = filter (\ rout -> routineAlias rout == Just opName &&
                                length (routineArgs rout) == numArgs) fs
    in listToMaybe ffs

findFeature :: ClassFeature a body expr => 
               AbsClas body expr -> String -> Maybe a
findFeature clasInt name = 
  let fs = filter (\f -> map toLower (featureName f) == map toLower name) 
                  (allFeatures clasInt)
  in listToMaybe fs

findFeatureEx :: AbsClas body expr -> String -> Maybe FeatureEx
findFeatureEx = findFeature

findRoutineInt :: ClasInterface -> String -> Maybe RoutineI
findRoutineInt = findFeature

findAttrInt :: AbsClas body Expr -> String -> Maybe (Attribute Expr)
findAttrInt = findFeature    

findConstantInt :: AbsClas body Expr -> String -> Maybe (Constant Expr)
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
makeGenericStub (Generic g constrs _) = 
  AbsClas { deferredClass = False
          , frozenClass = False
          , expandedClass = False
          , classNote  = []
          , className  = g
          , currProc   = Dot
          , procGeneric = []
          , obsoleteClass = False
          , procExpr   = []
          , generics   = []
          , inherit    = [Inheritance False $ map simpleInherit constrs]
          , creates    = []
          , converts   = []
          , featureClauses   = []
          , invnts     = []
          }
  where
    simpleInherit t = InheritClause t [] [] [] [] []
                  
-- Inheritance utilities

renameDecl :: RenameClause -> Decl -> Decl
renameDecl r@(Rename orig new _) (Decl n t)
  | n == orig = Decl new t'
  | otherwise = Decl n t'
  where
    t' = renameType r t

renameType r (ClassType n ts) = ClassType n (map (renameType r) ts)
renameType (Rename orig new _) (Like i) 
  | i == orig = Like new
  | otherwise = Like i

renameAll :: [RenameClause] -> AbsClas body exp -> AbsClas body exp
renameAll renames cls = foldr rename cls renames
  where
    rename r = 
      classMapConstants (flip featureRename r) .
      classMapAttributes (flip featureRename r) .
      classMapRoutines (flip featureRename r)


undefineName :: String -> AbsClas body exp -> AbsClas body exp
undefineName name cls = 
  let undefineClause (FeatureClause exps routs attrs consts)  = 
        FeatureClause exps (filterFeature routs)
                           (filterFeature attrs)
                           (filterFeature consts)
        
      filterFeature :: Feature a => [a] -> [a]
      filterFeature = filter ( (/= name) . featureName)
  in cls { featureClauses = map undefineClause (featureClauses cls)}

undefineAll :: InheritClause -> AbsClas body exp -> AbsClas body exp
undefineAll inh cls = foldr undefineName cls (undefine inh)


mergeableClass :: AbsClas body exp -> Bool
mergeableClass clas = True -- null (generics clas) -- && null (inherit clas)

mergeClass :: AbsClas body exp -> AbsClas body exp -> AbsClas body exp
mergeClass class1 class2 
  | mergeableClass class1 && mergeableClass class2 = 
      class1 { invnts = invnts class1 ++ invnts class2
             , featureClauses = featureClauses class1 ++ featureClauses class2
             }
  -- | not (mergeableClass class1) = error "mergeClasses: class1 not mergeable"
  -- | not (mergeableClass class2) = error "mergeClasses: class2 not mergeable"
  | otherwise = error $ "mergeClasses: classes not mergeable " ++ 
       show (className class1, className class2)

mergeClasses :: [AbsClas body exp] -> AbsClas body exp
mergeClasses = foldr1 mergeClass

-- Routine level utilities
makeRoutineI :: AbsRoutine body Expr -> RoutineI
makeRoutineI f = f { routineImpl = EmptyBody 
                   , routineRescue = Nothing}

argMap :: RoutineWithBody a -> Map String Typ
argMap = declsToMap . routineArgs

localMap :: RoutineWithBody a -> Map String Typ
localMap = declsToMap . routineDecls

routineDecls :: AbsRoutine (RoutineBody exp1) exp -> [Decl]
routineDecls r =
  case routineImpl r of
    RoutineDefer -> []
    RoutineExternal _ _ -> []
    body -> routineLocal body


updFeatBody :: RoutineBody a -> PosAbsStmt b -> RoutineBody b
updFeatBody impl body = impl {routineBody = body}

-- Operator utilities

-- Operator aliases for user-level operators, ie, not including
-- =, /=, ~, and /~
opAlias :: BinOp -> String
opAlias Add = "+"
opAlias Sub = "-"
opAlias Mul = "*"
opAlias Div = "/"
opAlias Quot = "//"
opAlias Rem = "\\"
opAlias Pow = "^"
opAlias And = "and"
opAlias AndThen = "and then"
opAlias Or = "or"
opAlias OrElse = "or else"
opAlias Xor = "xor"
opAlias Implies = "implies"
opAlias (SymbolOp o) = o
opAlias (RelOp o _) = rel o
  where
    rel Lte = "<="
    rel Lt = "<"
    rel Gt = ">"
    rel Gte = ">="    
    rel relOp = error $ "opAlias: non user-level operator " ++ show relOp
    
equalityOp :: BinOp -> Bool
equalityOp (RelOp Eq _) = True
equalityOp (RelOp Neq _) = True
equalityOp (RelOp TildeEq _) = True
equalityOp (RelOp TildeNeq _) = True
equalityOp _ = False


-- Unary operator aliases for everything except `old'.
unOpAlias Not = "not"
unOpAlias Neg = "-"
unOpAlias Old = "unOpAlias: `old' is not a user-operator."


-- Type utilities

classToType :: AbsClas body exp -> Typ
classToType clas = ClassType (className clas) (map genType (generics clas))
  where genType g = ClassType (genericName g) []

isBasic :: Typ -> Bool
isBasic t = any ($ t) [isIntegerType, isNaturalType, isRealType, isCharType]

typeBounds :: Typ -> (Integer, Integer)
typeBounds (ClassType n []) = fromJust $ lookup n wholeMap
  where
    intMap = zip integerTypeNames 
                 (map (\bits -> let half = bits `quot` 2
                                in (- 2^half, 2^half - 1)) [8,16,32,64])
    natMap = zip naturalTypeNames 
                 (map (\bits -> (0, 2^bits - 1)) [8,16,32,64])
    wholeMap = intMap ++ natMap

isIntegerType :: Typ -> Bool
isIntegerType = isInTypeNames integerTypeNames

isNaturalType :: Typ -> Bool
isNaturalType = isInTypeNames naturalTypeNames

isRealType :: Typ -> Bool
isRealType = isInTypeNames realTypeNames

isCharType :: Typ -> Bool
isCharType = isInTypeNames charTypeNames

isInTypeNames names (ClassType name _) = name `elem` names
isInTypeNames _ _ = False

integerTypeNames :: [String]
integerTypeNames = map (("INTEGER_" ++) . show) [8, 16, 32, 64]

naturalTypeNames :: [String]
naturalTypeNames = map (("NATURAL_" ++) . show) [8, 16, 32, 64]

realTypeNames :: [String]
realTypeNames = ["REAL_32", "REAL_64"]

charTypeNames :: [String]
charTypeNames = ["CHARACTER_8", "CHARACTER_32"]

classNameType :: Typ -> String
classNameType (ClassType cn _) = cn 
classNameType (Sep _ _ cn) = cn
classNameType _ = error "Non-class type"

intType :: Typ
intType = namedType "INTEGER_32"

boolType :: Typ
boolType = namedType "BOOLEAN"

realType :: Typ
realType = namedType "REAL_32"

charType :: Typ
charType = namedType "CHARACTER_8"

stringType :: Typ
stringType = namedType "STRING_8"

anyType :: Typ
anyType = namedType "ANY"
  
namedType :: ClassName -> Typ
namedType name = ClassType name []

-- Decl utilities
insertDecl :: Decl -> Map String Typ -> Map String Typ
insertDecl (Decl s t) = Map.insert s t

declsToMap :: [Decl] -> Map String Typ
declsToMap = foldr insertDecl Map.empty

-- SCOOP utilities

newVar :: ProcDecl -> Proc
newVar (SubTop   p) = p
newVar (CreateLessThan p _) = p
