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

-- | A 'Feature' can provide its name, arguments, contract, etc.
class Feature a expr | a -> expr where
  featureName     :: a -> String
  featureArgs     :: a -> [Decl]
  featureResult   :: a -> Typ
  featurePre      :: a -> [Clause expr]
  featurePost     :: a -> [Clause expr]
  featureIsFrozen :: a -> Bool
  featureRename   :: a -> RenameClause -> a

-- | An existential type to aggregate 
-- all features (routines, attributes, constants) together.
data FeatureEx expr = forall a. Feature a expr => FeatureEx a

instance Feature (FeatureEx expr) expr where
  featureName (FeatureEx f) = featureName f
  featureArgs (FeatureEx f) = featureArgs f
  featureResult (FeatureEx f) = featureResult f
  featurePre (FeatureEx f) = featurePre f
  featurePost (FeatureEx f) = featurePost f
  featureIsFrozen (FeatureEx f) = featureIsFrozen f
  featureRename (FeatureEx f) = FeatureEx . featureRename f

instance Feature (AbsRoutine body expr) expr where
  featureName = routineName
  featureArgs = routineArgs
  featureResult = routineResult
  featurePre = contractClauses . routineReq
  featurePost = contractClauses . routineEns
  featureIsFrozen = routineFroz
  featureRename rout r@(Rename orig new alias)
    | routineName rout == orig = rout { routineName = new
                                      , routineAlias = alias
                                      , routineArgs = newArgs
                                      } 
    | otherwise = rout {routineArgs = newArgs}
    where newArgs = map (renameDecl r) (routineArgs rout)

instance Feature (Attribute expr) expr where
  featureName = declName . attrDecl
  featureArgs = const []
  featureResult = declType . attrDecl
  featurePre = contractClauses . attrReq
  featurePost = contractClauses . attrEns
  featureIsFrozen = attrFroz
  featureRename attr r =
    attr {attrDecl = renameDecl r (attrDecl attr)}

instance Feature (Constant expr) expr where
  featureName = declName . constDecl
  featureArgs = const []
  featureResult = declType . constDecl
  featurePre _ = []
  featurePost _ = []
  featureIsFrozen = constFroz
  featureRename constant r =
    constant {constDecl = renameDecl r (constDecl constant)}

-- | A way to extract each type of feature from a class.
class Feature a expr => ClassFeature a body expr | a -> expr, a -> body where
  allFeatures :: AbsClas body expr -> [a]
  
instance ClassFeature (Constant expr) body expr where
  allFeatures = allConstants
  
instance ClassFeature (AbsRoutine body expr) body expr where
  allFeatures = allRoutines

instance ClassFeature (Attribute expr) body expr where
  allFeatures = allAttributes

instance ClassFeature (FeatureEx expr) body expr where
  allFeatures clas = map FeatureEx (allAttributes clas) ++
                     map FeatureEx (allRoutines clas) ++
                     map FeatureEx (allConstants clas)
     

-- | Convert a constant into an attribute.
constToAttr :: Constant exp -> Attribute Expr
constToAttr (Constant froz d _) = 
  Attribute froz d Nothing [] (Contract False []) (Contract False [])

-- | Fetch attributes from all feature clauses.
allAttributes = concatMap attributes . featureClauses

-- | Fetch routines from all feature clauses.
allRoutines = concatMap routines . featureClauses

-- | Fetch contants from all feature clauses.
allConstants = concatMap constants . featureClauses

-- | Fetch creation routines from all feature clauses.
allCreates = concatMap createNames . creates


-- | Fetch attribute declarations from all feature clauses.
allAttributeDecls = map attrDecl . allAttributes

-- | Fetch constant declarations from all feature clauses.
allConstantDecls = map constDecl . allConstants


-- | All inheritance clauses.
allInherited = concatMap inheritClauses . inherit

-- | All inherited classes, as types.
allInheritedTypes = map inheritClass . allInherited

-- | Determine if a name is in the creation clause of a class.
isCreateName n c = n `elem` allCreates c

-- | Map a transformation function over the routines in a class, replacing the 
-- old routines with the transformed versions.
mapRoutines f clause = clause {routines = map f (routines clause)}

-- | Monadic version of 'mapRoutines'.
mapRoutinesM :: Monad m =>
                (AbsRoutine body exp -> m (AbsRoutine body exp)) ->
                FeatureClause body exp -> 
                m (FeatureClause body exp)
mapRoutinesM f clause = do
  routs <- mapM f (routines clause)
  return (clause {routines = routs})

-- | Map a transformation function over the attributes in a class, 
-- replacing the old attributes with the transformed versions.
mapAttributes f clause = clause {attributes = map f (attributes clause)}

-- | Monadic version of 'mapAttributes'.
mapAttributesM :: Monad m =>
                  (Attribute exp -> m (Attribute exp)) ->
                  FeatureClause body exp -> 
                  m (FeatureClause body exp)
mapAttributesM f clause = do
  attrs <- mapM f (attributes clause)
  return (clause {attributes = attrs})

-- | Map a transformation function over the constants in a class, replacing the
-- old constants with the transformed versions.
mapConstants f clause = clause {constants = map f (constants clause)}

-- | Map a transformation function over the contracts in a class, replacing the
-- old contracts with the transformed versions.
mapContract clauseF cs =
  cs { contractClauses = map clauseF (contractClauses cs)}

-- | Map a transformation function over all expressions in a class. 
-- A transformation for features, constants, and attributes must be given
-- as if the type of expressions changes (ie, with a typecheck) then
-- all expressions types must change together.
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

findFeatureEx :: AbsClas body expr -> String -> Maybe (FeatureEx expr)
findFeatureEx = findFeature

findRoutineInt :: ClasInterface -> String -> Maybe RoutineI
findRoutineInt = findFeature

findAttrInt :: AbsClas body expr -> String -> Maybe (Attribute expr)
findAttrInt = findFeature    

findConstantInt :: AbsClas body Expr -> String -> Maybe (Constant Expr)
findConstantInt = findFeature 

updFeatureClauses :: AbsClas body exp -> [FeatureClause body exp] 
                     -> AbsClas body exp
updFeatureClauses c fcs = c {featureClauses = fcs}

fullName :: AbsClas body exp -> RoutineI -> String
fullName c f = fullNameStr (className c) (routineName f)

fullNameStr :: String -> String -> String
fullNameStr cName fName = "__" ++ cName ++ "_" ++ fName

genericStubs :: AbsClas body exp -> [AbsClas body' exp']
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
        
      filterFeature :: Feature a expr => [a] -> [a]
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

-- | Convert a class into its type.
classToType :: AbsClas body exp -> Typ
classToType clas = ClassType (className clas) (map genType (generics clas))
  where genType g = ClassType (genericName g) []

-- | Whether a type is basic (where basic meaning its an integer, natural, real or boolean).
isBasic :: Typ -> Bool
isBasic t = any ($ t) [isBooleanType, isIntegerType, isNaturalType, isRealType, isCharType]

-- | The bounds on the range of values a integer or natural type can take.
typeBounds :: Typ -> (Integer, Integer)
typeBounds (ClassType n []) = fromJust $ lookup n wholeMap
  where
    intMap = zip integerTypeNames 
                 (map (\bits -> let half = bits `quot` 2
                                in (- 2^half, 2^half - 1)) [8,16,32,64])
    natMap = zip naturalTypeNames 
                 (map (\bits -> (0, 2^bits - 1)) [8,16,32,64])
    wholeMap = intMap ++ natMap

isBooleanType :: Typ -> Bool
isBooleanType = (== "BOOLEAN") . classNameType

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
realType = namedType "REAL_64"

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
