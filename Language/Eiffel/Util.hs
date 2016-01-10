{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Eiffel.Util where

import           Control.Applicative hiding (getConst)
import           Control.Monad
import           Control.Lens hiding (from, lens)

import           Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.Eiffel.Syntax
import           Language.Eiffel.Position

-- Class level utilities

-- | A 'Feature' can provide its name, arguments, contract, etc.
class Feature a expr | a -> expr where
  -- | The name of the feature.
  featureName     :: a -> Text

  -- | Argument declarations.
  featureArgs     :: a -> [Decl]

  -- | Result type.
  featureResult   :: a -> Typ

  -- | Precondition.
  featurePre      :: a -> [Clause expr]

  -- | Postconditions.
  featurePost     :: a -> [Clause expr]

  -- | Whether the feature is frozen (can't be redefined).
  featureIsFrozen :: a -> Bool

  -- | Transform the feature given a renaming clause.
  featureRename   :: a -> RenameClause -> a

-- | An existential type to aggregate
-- all features (routines, attributes, constants) together.
data FeatureEx expr =
  -- | Wrap the 'Feature' in the existential type.
  forall a. Feature a expr => FeatureEx a

instance Feature (FeatureEx expr) expr where
  featureName (FeatureEx f) = featureName f
  featureArgs (FeatureEx f) = featureArgs f
  featureResult (FeatureEx f) = featureResult f
  featurePre (FeatureEx f) = featurePre f
  featurePost (FeatureEx f) = featurePost f
  featureIsFrozen (FeatureEx f) = featureIsFrozen f
  featureRename (FeatureEx f) = FeatureEx . featureRename f

-- | Wrap up SomeFeature

wrapSomeFeature :: (forall f . Feature f expr => f -> b)
                   -> SomeFeature body expr
                   -> b
wrapSomeFeature f (SomeRoutine r) = f r
wrapSomeFeature f (SomeAttr a) = f a
wrapSomeFeature f (SomeConst c) = f c

instance Feature (SomeFeature body expr) expr where
  featureName = wrapSomeFeature featureName
  featureArgs = wrapSomeFeature featureArgs
  featureResult = wrapSomeFeature featureResult
  featurePre = wrapSomeFeature featurePre
  featurePost = wrapSomeFeature featurePost
  featureIsFrozen = wrapSomeFeature featureIsFrozen
  featureRename (SomeRoutine rout) r = SomeRoutine (featureRename rout r)
  featureRename (SomeAttr attr) r = SomeAttr (featureRename attr r)
  featureRename (SomeConst c) r = SomeConst (featureRename c r)

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
class Feature a expr => ClassFeature a body expr where
  -- | A list of all this class' features of the given type.
  allFeatures :: AbsClas body expr -> [a]

  -- | Find this kind of feature in a class
  findFeature :: AbsClas body expr -> Text -> Maybe a

instance ClassFeature (Constant expr) body expr where
  findFeature = findFeature' toConstMb
  allFeatures = allConstants

instance ClassFeature (AbsRoutine body expr) body expr where
  findFeature = findFeature' toRoutineMb
  allFeatures = allRoutines

instance ClassFeature (Attribute expr) body expr where
  findFeature = findFeature' toAttrMb
  allFeatures = allAttributes

instance ClassFeature (FeatureEx expr) body expr where
  allFeatures clas = map FeatureEx (allAttributes clas) ++
                     map FeatureEx (allRoutines clas) ++
                     map FeatureEx (allConstants clas)
  findFeature = findFeature' (Just . FeatureEx)


findFeature' :: (SomeFeature body expr -> Maybe a)
                -> AbsClas body expr
                -> Text
                -> Maybe a
findFeature' from cls name = join $ from <$> findSomeFeature cls name

-- | Convert a constant into an attribute.
constToAttr :: Constant exp -> Attribute Expr
constToAttr (Constant froz d _) =
  Attribute froz d Nothing [] (Contract False []) (Contract False [])

-- * Extracting data from a class.

-- | Fetch attributes from all feature clauses.
allAttributes = allHelper fmAttrs

-- | Fetch routines from all feature clauses.
allRoutines = allHelper fmRoutines

-- | Fetch contants from all feature clauses.
allConstants = allHelper fmConsts

-- Help for above 'all' functions
allHelper lens =
  map (view exportFeat) . Map.elems . view lens . featureMap

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

-- * 'SomeFeature' predicates, extractors.

toRoutineMb (SomeRoutine r) = Just r
toRoutineMb _ = Nothing

toAttrMb (SomeAttr r) = Just r
toAttrMb _ = Nothing

toConstMb (SomeConst r) = Just r
toConstMb _ = Nothing

-- isRoutine (SomeRoutine _) = True
-- isRoutine _ = False

-- isAttr (SomeAttr _) = True
-- isAttr _ = False

-- isConst (SomeConst _) = True
-- isConst _ = False

-- getRoutine (SomeRoutine r) = r

-- getAttr (SomeAttr a) = a

-- getConst (SomeConst c) = c

-- onlyRoutine f (SomeRoutine a) = SomeRoutine (f a)
-- onlyRoutine _f other = other

-- onlyAttr f (SomeAttr a) = SomeAttr (f a)
-- onlyAttr _f other = other

-- onlyConst f (SomeConst a) = SomeConst (f a)
-- onlyConst _f other = other

-- onlyRoutineM f (SomeRoutine a) = SomeRoutine <$> f a
-- onlyRoutineM f other = pure other

-- onlyAttrM :: Applicative m
--              => (Attribute exp -> m (Attribute exp))
--              -> SomeFeature body exp
--              -> m (SomeFeature body exp)
-- onlyAttrM f (SomeAttr a) = SomeAttr <$> f a
-- onlyAttrM f other = pure other

-- onlyConstM f (SomeConst a) = SomeConst (f a)
-- onlyConstM f other = other



-- * Class modification

-- ** Setting members of a class.

-- | Set the feature clause of a class.
updFeatureMap :: AbsClas body exp -> FeatureMap body exp -> AbsClas body exp
updFeatureMap c featMap = c {featureMap = featMap}

-- | Update a routine body.
updFeatBody :: RoutineBody a -> PosAbsStmt b -> RoutineBody b
updFeatBody impl body = impl {routineBody = body}

-- ** Mapping features of a class
-- | These functions will update a class or feature clause with a transformation
-- function.

-- | Map a transformation function over the routines in a class, replacing the
-- old routines with the transformed versions within a feature clause.
-- mapRoutines f  = Map.map (over exportFeat (onlyRoutine f))

-- | Monadic version of 'mapRoutines'.
mapRoutinesM :: (Applicative m, Monad m) =>
                (AbsRoutine body exp -> m (AbsRoutine body exp)) ->
                FeatureMap body exp ->
                m (FeatureMap body exp)
mapRoutinesM f = mapMOf (fmRoutines.traverse.exportFeat) f

-- | Map a transformation function over the attributes in a class,
-- replacing the old attributes with the transformed versions within a feature clause.
mapAttributes :: (Attribute exp -> Attribute exp)
                 -> FeatureMap body exp
                 -> FeatureMap body exp
mapAttributes f = over (fmAttrs.traverse.exportFeat) f

-- | Monadic version of 'mapAttributes'.
mapAttributesM :: (Monad m, Applicative m) =>
                  (Attribute exp -> m (Attribute exp)) ->
                  FeatureMap body exp ->
                  m (FeatureMap body exp)
mapAttributesM f = mapMOf (fmAttrs.traverse.exportFeat) f
-- | Map a transformation function over the constants in a class, replacing the
-- old constants with the transformed versions within a feature clause.
-- mapConstants f = Map.map (over exportFeat (onlyConst f))

-- | Map a transformation function over the contracts in a class, replacing the
-- old contracts with the transformed versions within a feature clause.
mapContract clauseF cs =
  cs { contractClauses = map clauseF (contractClauses cs)}

-- | Map a transformation function over all expressions in a class.
-- A transformation for features, constants, and attributes must be given
-- as if the type of expressions changes (ie, with a typecheck) then
-- all expressions types must change together.
mapExprs :: (AbsRoutine body exp -> AbsRoutine body' exp')
            -> (Constant exp -> Constant exp')
            -> (Clause exp -> Clause exp')
            -> FeatureMap body exp
            -> FeatureMap body' exp'
mapExprs routF constF clauseF fm =
  FeatureMap (mapUpd routF fmRoutines)
             (mapUpd updAttr fmAttrs)
             (mapUpd constF fmConsts)
  where
    updAttr a = a { attrEns = mapContract clauseF (attrEns a)
                  , attrReq = mapContract clauseF (attrReq a)
                  }

    mapUpd f lens = Map.map (over exportFeat f) (view lens fm)


-- | Map a transformation function over the attributes in a class, replacing the
-- old attributes with the transformed versions within a class.
classMapAttributes f c =
  c {featureMap = mapAttributes f (featureMap c)}

-- | Monadic version of 'classMapAttributes'.
classMapAttributesM :: (Applicative m, Monad m) =>
                       (Attribute exp -> m (Attribute exp)) ->
                       AbsClas body exp ->
                       m (AbsClas body exp)
classMapAttributesM f c = do
  fm <- mapAttributesM f (featureMap c)
  return (c {featureMap = fm})

-- | Map a transformation function over the routines in a class, replacing the
-- old routines with the transformed versions within a class.
classMapRoutines :: (AbsRoutine body exp -> AbsRoutine body exp)
                    -> AbsClas body exp -> AbsClas body exp
classMapRoutines f c =
  c {featureMap = over (fmRoutines.traverse.exportFeat) f (featureMap c)}

-- | Monadic version of 'classMapRoutines'.
classMapRoutinesM :: (Applicative m, Monad m) =>
                     (AbsRoutine body exp -> m (AbsRoutine body exp)) ->
                     AbsClas body exp ->
                     m (AbsClas body exp)
classMapRoutinesM f c = do
  fm <- mapRoutinesM f (featureMap c)
  return (c {featureMap = fm})

-- | Map a transformation function over the constants in a class, replacing the
-- old constants with the transformed versions within a class.
classMapConstants f c =
  c {featureMap = over (fmConsts.traverse.exportFeat) f (featureMap c)}

-- | Map a transformation function over all expressions in a class.
-- A transformation for features, constants, and attributes must be given
-- as if the type of expressions changes (ie, with a typecheck) then
-- all expressions types must change together. This is performed on every
-- feature clause in a class.
classMapExprs :: (AbsRoutine body exp -> AbsRoutine body' exp')
                 -> (Clause exp -> Clause exp')
                 -> (Constant exp -> Constant exp')
                 -> AbsClas body exp -> AbsClas body' exp'
classMapExprs featrF clauseF constF c =
  c { featureMap = mapExprs featrF constF clauseF (featureMap c)
    , invnts     = map clauseF (invnts c)
    }

-- * Interface construction

-- | Strip the body from a routine.
makeRoutineIs :: SomeFeature body Expr -> SomeFeature EmptyBody Expr
makeRoutineIs (SomeRoutine r) = SomeRoutine (makeRoutineI r)
makeRoutineIs (SomeAttr a) = SomeAttr a
makeRoutineIs (SomeConst c) = SomeConst c

-- | Strip the contracts from an attribute.
makeAttributeI :: Attribute exp -> Attribute Expr
makeAttributeI (Attribute froz decl assgn notes _ _) =
  Attribute froz decl assgn notes (Contract False []) (Contract False [])

-- | Strip the bodies from all features.
clasInterface :: AbsClas body Expr -> ClasInterface
clasInterface c =
  c { featureMap = over (fmRoutines.traverse.exportFeat)
                        (makeRoutineI)
                        (featureMap c)
    }

-- | Strip the bodies and rescue clause from a routine.
makeRoutineI :: AbsRoutine body Expr -> RoutineI
makeRoutineI f = f { routineImpl = EmptyBody
                   , routineRescue = Nothing}

-- * Map construction

-- | Turn a list of classes into a map indexed by the class names.
clasMap :: [AbsClas body exp] -> Map ClassName (AbsClas body exp)
clasMap = Map.fromList . map (\ c -> (className c, c))

-- | Extract a map of attribute names to types given a class.
attrMap :: AbsClas body exp -> Map Text Typ
attrMap = declsToMap . map attrDecl . allAttributes

-- * Search

-- | Find a routine in a class.
findRoutine :: Clas -> Text -> Maybe Routine
findRoutine = findFeature

-- | Find an operator (symbol sequence) in a class.
findOperator :: AbsClas body Expr -> Text -> Int ->
                Maybe (AbsRoutine body Expr)
findOperator c opName numArgs =
    let fs = allRoutines c
        ffs = filter (\ rout -> routineAlias rout == Just opName &&
                                length (routineArgs rout) == numArgs) fs
    in listToMaybe ffs

-- -- | Find a 'ClassFeature'.
-- findFeature :: ClassFeature a body expr =>
--                AbsClas body expr -> Text -> Maybe a
-- findFeature clasInt name =
--   let fs = filter (\f -> Text.map toLower (featureName f) == Text.map toLower name)
--                   (allFeatures clasInt)
--   in listToMaybe fs

-- | Find the sum-type for all features.
findSomeFeature :: AbsClas body expr -> Text -> Maybe (SomeFeature body expr)
findSomeFeature cls name =
  lkup fmRoutines SomeRoutine <|>
  lkup fmAttrs SomeAttr <|>
  lkup fmConsts SomeConst
  where
    lkup lens cast = cast <$>
                     view exportFeat <$>
                     Map.lookup nameLow (view lens featMap)
    featMap = featureMap cls
    nameLow = Text.toLower name

-- | Find an existential 'FeatureEx'.
findFeatureEx :: AbsClas body expr -> Text -> Maybe (FeatureEx expr)
findFeatureEx = findFeature

-- | Find a routine by name.
findRoutineInt :: ClasInterface -> Text -> Maybe RoutineI
findRoutineInt = findFeature

-- | Find an attribute in a class by name.
findAttrInt :: AbsClas body expr -> Text -> Maybe (Attribute expr)
findAttrInt = findFeature

-- | Find a constant by name in a class.
findConstantInt :: AbsClas body Expr -> Text -> Maybe (Constant Expr)
findConstantInt = findFeature

-- | Given a class and a routine, given a unique name.
fullName :: AbsClas body exp -> RoutineI -> Text
fullName c f = fullNameStr (className c) (routineName f)

-- | Given to string construct a unique combination.
fullNameStr :: Text -> Text -> Text
fullNameStr cName fName = Text.concat ["__", cName, "_", fName]

-- | Given a class, create a list of generic classes for the formal generic
-- parameters of the class.
genericStubs :: AbsClas body exp -> [AbsClas body' exp']
genericStubs = map makeGenericStub . generics

-- | Given a generic, construct a class for the generic.
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
          , featureMap = FeatureMap Map.empty Map.empty Map.empty
          , invnts     = []
          }
  where
    simpleInherit t = InheritClause t [] [] [] [] []

-- * Inheritance utilities

-- | Rename a declaration.
renameDecl :: RenameClause -> Decl -> Decl
renameDecl r@(Rename orig new _) (Decl n t)
  | n == orig = Decl new t'
  | otherwise = Decl n t'
  where
    t' = renameType r t

-- | Rename a type, in the case of a like-type.
renameType r (ClassType n ts) = ClassType n (map (renameType r) ts)
renameType (Rename orig new _) (Like (Pos sn l c (UnqualCall i [])))
  | i == orig = Like (Pos sn l c (UnqualCall new []))
  | otherwise = Like (Pos sn l c (UnqualCall i []))
renameType r t = error $ "renameType: rename " ++ show r ++
                         " in type: " ++ show t

-- | Rename everything in a class.
renameAll :: [RenameClause] -> AbsClas body exp -> AbsClas body exp
renameAll renames cls = renamed
  where
    renamed = foldr renameClass cls renames

    renameKey (Rename old new _aliasMb) k
      | k == Text.toLower old  = new
      | otherwise             = k
    renameKeys r c = c { featureMap = fmMapKeys (renameKey r) (featureMap c)}
    renameClass r = renameKeys r .
      classMapConstants (flip featureRename r) .
      classMapAttributes (flip featureRename r) .
      classMapRoutines (flip featureRename r)

-- | Undefine a single feature in a class.
undefineName :: Text -> AbsClas body exp -> AbsClas body exp
undefineName name cls =
  cls { featureMap = fmKeyFilter (/= name) (featureMap cls)}

-- | Undefine every specified name for a class.
undefineAll :: InheritClause -> AbsClas body exp -> AbsClas body exp
undefineAll inh cls = foldr undefineName cls (undefine inh)

-- | Specifies whether a class can be merged with another.
mergeableClass :: AbsClas body exp -> Bool
mergeableClass _clas = True -- null (generics clas) -- && null (inherit clas)

-- | Merge two classes, combining invariants and feature clauses.
mergeClass :: AbsClas body exp -> AbsClas body exp -> AbsClas body exp
mergeClass class1 class2
  | mergeableClass class1 && mergeableClass class2 =
      class1 { invnts = invnts class1 ++ invnts class2
             , featureMap = featureMap class1 `fmUnion` featureMap class2
             }
  | otherwise = error $ "mergeClasses: classes not mergeable " ++
       show (className class1, className class2)

-- | Merge a list of classes.
mergeClasses :: [AbsClas body exp] -> AbsClas body exp
mergeClasses = foldr1 mergeClass


-- * Feature Map functions
fmMapKeys :: (Text -> Text) -> FeatureMap body exp -> FeatureMap body exp
fmMapKeys f = fmKeyMap fmRoutines . fmKeyMap fmAttrs . fmKeyMap fmConsts
  where
    fmKeyMap setter = over setter mapKeys

    mapKeys :: Map Text v -> Map Text v
    mapKeys = Map.fromList . map (\(k,v) -> (f k, v)) . Map.toList

fmKeyFilter :: (Text -> Bool)
               -> FeatureMap body exp
               -> FeatureMap body exp
fmKeyFilter p = fmFilt fmRoutines . fmFilt fmAttrs . fmFilt fmConsts
  where
    fmFilt setter = over setter filt
    filt = Map.filterWithKey (\ k _v -> p k)

fmUnion
  :: FeatureMap body exp
  -> FeatureMap body exp
  -> FeatureMap body exp
fmUnion fm1 fm2 =
  FeatureMap
    (Map.union (view fmRoutines fm1) (view fmRoutines fm2))
    (Map.union (view fmAttrs fm1) (view fmAttrs fm2))
    (Map.union (view fmConsts fm1) (view fmConsts fm2))

fmEmpty = FeatureMap Map.empty Map.empty Map.empty

fmUnions = foldr fmUnion fmEmpty

-- * Routine level utilities

-- | Construct a map from a routine's arguments.
argMap :: RoutineWithBody a -> Map Text Typ
argMap = declsToMap . routineArgs

-- | Construct a map from a routine's declarations.
localMap :: RoutineWithBody a -> Map Text Typ
localMap = declsToMap . routineDecls

-- | Give the declarations of a routine's locals.
routineDecls :: AbsRoutine (RoutineBody exp1) exp -> [Decl]
routineDecls r =
  case routineImpl r of
    RoutineDefer -> []
    RoutineExternal _ _ -> []
    body -> routineLocal body

-- Operator utilities

-- | Operator aliases for user-level operators, ie, not including
-- =, /=, ~, and /~
opAlias :: BinOp -> Text
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

-- | Test if the binary operator is an equality operator.
equalityOp :: BinOp -> Bool
equalityOp (RelOp Eq _) = True
equalityOp (RelOp Neq _) = True
equalityOp (RelOp TildeEq _) = True
equalityOp (RelOp TildeNeq _) = True
equalityOp _ = False


-- | Unary operator aliases for everything except `old'.
unOpAlias Not = "not"
unOpAlias Neg = "-"
unOpAlias Old = "unOpAlias: `old' is not a user-operator."


-- * Type utilities

-- | Convert a class into its type.
classToType :: AbsClas body exp -> Typ
classToType clas = ClassType (className clas) (map genType (generics clas))
  where genType g = ClassType (genericName g) []

-- | Whether a type is basic (where basic meaning its an integer, natural, real or boolean).
isBasic :: Typ -> Bool
isBasic t = any ($ t) [isBooleanType, isIntegerType, isNaturalType, isRealType, isCharType]

-- | A list of the number of integer bits (8, 16, ...)
intBits :: [Integer]
intBits = [8, 16, 32, 64]


-- | The bounds on the range of values a integer or natural type can take.
typeBounds :: Typ -> (Integer, Integer)
typeBounds (ClassType n []) = fromJust $ lookup n wholeMap
  where
    intMap = zip integerTypeNames
                 (map (\bits -> let half = bits `quot` 2
                                in (- 2^half, 2^half - 1)) intBits)
    natMap = zip naturalTypeNames
                 (map (\bits -> (0, 2^bits - 1)) intBits)
    wholeMap = intMap ++ natMap
typeBounds t = error $ "typeBounds: won't work on " ++ show t

-- | Boolean type test.
isBooleanType :: Typ -> Bool
isBooleanType = (== "BOOLEAN") . classNameType

-- | Integer type test.
isIntegerType :: Typ -> Bool
isIntegerType = isInTypeNames integerTypeNames

-- | Natural number type test.
isNaturalType :: Typ -> Bool
isNaturalType = isInTypeNames naturalTypeNames

-- | Real number type test.
isRealType :: Typ -> Bool
isRealType = isInTypeNames realTypeNames

-- | Character type test.
isCharType :: Typ -> Bool
isCharType = isInTypeNames charTypeNames

isInTypeNames names (ClassType name _) = name `elem` names
isInTypeNames _ _ = False

-- | List of integer type names (ie, INTEGER_32).
integerTypeNames :: [Text]
integerTypeNames = map ((Text.append "INTEGER_") . Text.pack . show) intBits

-- | List of integer type names (ie, NATURAL_32).
naturalTypeNames :: [Text]
naturalTypeNames = map ((Text.append "NATURAL_") . Text.pack . show) intBits

-- | List of integer type names (ie, REAL_64).
realTypeNames :: [Text]
realTypeNames = ["REAL_32", "REAL_64"]

-- | List of integer type names (ie, CHARACTER_8).
charTypeNames :: [Text]
charTypeNames = ["CHARACTER_8", "CHARACTER_32"]

-- | Given a type give the name of the class as a string.
classNameType :: Typ -> Text
classNameType (ClassType cn _) = cn
classNameType (Sep _ _ cn) = cn
classNameType t = error $ "Non-class type " ++ show t

-- | The default integer type.
intType :: Typ
intType = namedType "INTEGER_32"

-- | The default boolean type.
boolType :: Typ
boolType = namedType "BOOLEAN"

-- | The default real number type.
realType :: Typ
realType = namedType "REAL_64"

-- | The default character type.
charType :: Typ
charType = namedType "CHARACTER_8"

-- | The default string type.
stringType :: Typ
stringType = namedType "STRING_8"

-- | The top type, ANY.
anyType :: Typ
anyType = namedType "ANY"

-- | Construct a simple type from a classname.
namedType :: ClassName -> Typ
namedType name = ClassType name []

-- * Declaration

-- | Insert a declaration into a string-type map.
insertDecl :: Decl -> Map Text Typ -> Map Text Typ
insertDecl (Decl s t) = Map.insert s t

-- | Turn a list of declarations into a string-type map.
declsToMap :: [Decl] -> Map Text Typ
declsToMap = foldr insertDecl Map.empty

-- * SCOOP utilities

-- | Given a processor declaration, extract the processor.
newVar :: ProcDecl -> Proc
newVar (SubTop   p) = p
newVar (CreateLessThan p _) = p
