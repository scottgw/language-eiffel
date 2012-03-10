{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Eiffel.Syntax where

import Data.List
import qualified Data.Map as Map
import Data.Map (Map) 
import Data.Maybe (listToMaybe)

import Language.Eiffel.Position

type Clas = ClasBody Expr
type ClasBody exp = AbsClas RoutineBody exp
type ClasInterface = AbsClas EmptyBody Expr
type ClasI exp = AbsClas RoutineBody exp

data AbsClas (body :: * -> *) exp =
    AbsClas
    {
      frozenClass :: Bool,
      expandedClass :: Bool,
      deferredClass :: Bool,
      classNote  :: [Note],
      className  :: ClassName,
      currProc   :: Proc,
      procGeneric :: [Proc],
      procExpr   :: [ProcDecl],
      generics   :: [Generic],
      obsoleteClass :: Bool,
      inherit    :: [Inheritance],
      creates    :: [CreateClause],
      converts   :: [ConvertClause],
      featureClauses   :: [FeatureClause body exp],
      invnts     :: [Clause exp]
    } deriving (Eq, Show)

data Inheritance
     = Inheritance
       { inheritNonConform :: Bool
       , inheritClauses :: [InheritClause]
       } deriving (Show, Eq)

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
          , genericConstType :: [Typ]
          , genericCreate :: Maybe [String]
          } deriving (Show, Eq)

data CreateClause = 
  CreateClause { createExportNames :: [ClassName]
               , createNames :: [String]
               } deriving (Show, Eq)
		 
data ConvertClause = ConvertFrom String [Typ]
                   | ConvertTo String [Typ] deriving (Show, Eq)

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
allInheritedTypes = concatMap (map inheritClass . inheritClauses) . inherit
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



type RoutineI = AbsRoutine EmptyBody Expr
type RoutineWithBody exp = AbsRoutine RoutineBody exp
type Routine = RoutineWithBody Expr

data EmptyBody exp = EmptyBody deriving (Show, Eq)

data Contract exp = 
  Contract { contractInherited :: Bool 
           , contractClauses :: [Clause exp]
           } deriving (Show, Eq)

data AbsRoutine (body :: * -> *) exp = 
    AbsRoutine 
    { routineFroz   :: Bool
    , routineName   :: String
    , routineAlias  :: Maybe String
    , routineArgs   :: [Decl]
    , routineResult :: Typ
    , routineAssigner :: Maybe String
    , routineNote   :: [Note]
    , routineProcs  :: [Proc]
    , routineReq    :: Contract exp
    , routineReqLk  :: [ProcExpr]
    , routineImpl   :: body exp
    , routineEns    :: Contract exp
    , routineEnsLk  :: [Proc]
    , routineRescue :: Maybe [PosAbsStmt exp]
    } deriving (Show, Eq)

data RoutineBody exp 
  = RoutineDefer
  | RoutineExternal String (Maybe String)
  | RoutineBody 
    { routineLocal :: [Decl]
    , routineLocalProcs :: [ProcDecl]
    , routineBody  :: PosAbsStmt exp
    } deriving (Show, Eq)

data Attribute exp = 
  Attribute { attrFroz :: Bool 
            , attrDecl :: Decl
            , attrAssign :: Maybe String
            , attrNotes :: [Note]
            , attrReq :: Contract exp
            , attrEns :: Contract exp
            } deriving (Show, Eq)
  
data Constant exp = 
  Constant { constFroz :: Bool  
           , constDecl :: Decl
           , constVal :: exp
           } deriving (Show, Eq)  


class Feature a where
  featureName     :: a -> String
  featureResult   :: a -> Typ
  featureIsFrozen :: a -> Bool

data FeatureEx where
  FeatureEx :: Feature a => a -> FeatureEx

instance Feature FeatureEx where
  featureName (FeatureEx f) = featureName f
  featureResult (FeatureEx f) = featureResult f
  featureIsFrozen (FeatureEx f) = featureIsFrozen f

instance Feature (AbsRoutine body exp) where
  featureName = routineName
  featureResult = routineResult
  featureIsFrozen = routineFroz

instance Feature (Attribute exp) where
  featureName = declName . attrDecl
  featureResult = declType . attrDecl
  featureIsFrozen = attrFroz

instance Feature (Constant exp) where
  featureName = declName . constDecl
  featureResult = declType . constDecl
  featureIsFrozen = constFroz
 


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


type Expr = Pos UnPosExpr 

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Quot
           | Rem
           | Pow
           | Or
           | OrElse
           | Xor
           | And
           | AndThen
           | Implies
           | RelOp ROp Typ
           | SymbolOp String
             deriving (Show, Eq)

data ROp = Lte
         | Lt 
         | Eq 
         | TildeEq
         | Neq
         | TildeNeq
         | Gt 
         | Gte
           deriving (Show, Eq)

data UnOp = Not
          | Neg
          | Old
          | Sqrt
            deriving (Show, Eq)

data UnPosExpr =
    UnqualCall String [Expr]
  | QualCall Expr String [Expr]
  | Lookup Expr [Expr]
  | PrecursorCall (Maybe String) [Expr]
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  | Address Expr
  | Attached (Maybe Typ) Expr (Maybe String)
  | AcrossExpr Expr String Quant Expr
  | Agent Expr
  | CreateExpr Typ String [Expr]
  | Tuple [Expr]
  | InlineAgent [Decl] (Maybe Typ) [Stmt] [Expr]
  | ManifestCast Typ Expr
  | TypedVar String Typ
  | VarOrCall String
  | ResultVar
  | OnceStr String
  | CurrentVar
  | StaticCall Typ String [Expr]
  | LitArray [Expr]
  | LitString String
  | LitChar Char
  | LitInt Int
  | LitBool Bool
  | LitVoid
  | LitDouble Double 
  | LitType Typ deriving Eq

defaultCreate = "default_create"

data Quant = All | Some deriving (Eq, Show)

commaSepShow es = intercalate "," (map show es)
argsShow args = "(" ++ commaSepShow args ++ ")"

instance Show UnPosExpr where
    show (UnqualCall s args) = s ++ argsShow args
    show (QualCall t s args) = show t ++ "." ++ s ++ argsShow args
    show (Lookup t args) = show t ++ "[" ++ commaSepShow args ++ "]"
    show (PrecursorCall t args) = "Precursor " ++ show t ++  argsShow args
    show (BinOpExpr op e1 e2) 
        = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (UnOpExpr op e) = "(" ++ show op ++ " " ++ show e ++ ")"
    show (Attached s1 e s2) = "(attached " ++ show s1 ++ ", " ++ show e ++ " as " ++ show s2 ++ ")"
    show (CreateExpr t s args)
        = "create {" ++ show t ++ "}." ++ s ++ "(" ++ intercalate "," (map show args) ++ ")"
    show (TypedVar var t) = "(" ++ var ++ ": " ++ show t ++ ")"
    show (ManifestCast t e) = "{" ++ show t ++ "} " ++ show e
    show (StaticCall t i args) = "{" ++ show t ++ "}." ++ i ++ argsShow args
    show (Address e) = "$" ++ show e
    show (OnceStr s) = "once " ++ s
    show (VarOrCall s) = s
    show ResultVar  = "Result"
    show CurrentVar = "Current"
    show (LitString s) = "\"" ++ s ++ "\""
    show (LitChar c) = "'" ++ [c] ++ "'"
    show (LitInt i)  = show i
    show (LitBool b) = show b
    show (LitDouble d) = show d
    show (LitType t) = "({" ++ show t ++ "})"
    show (Tuple es) = show es
    show (LitArray es) = "<<" ++ commaSepShow es ++ ">>"
    show (Agent e)  = "agent " ++ show e
    show (InlineAgent ds r ss args) = 
      "agent " ++ show ds ++ ":" ++ show r ++ " " ++ show ss ++ " " ++ show args
    show LitVoid = "Void"




data Typ = ClassType ClassName [Typ]
         | IntType
         | TupleType (Either [Typ] [Decl])
         | Sep (Maybe Proc) [Proc] String
         | Like String
         | DoubleType
         | VoidType
         | NoType
         | BoolType deriving (Eq, Ord)

data Decl = Decl 
    { declName :: String,
      declType :: Typ
    } deriving (Ord, Eq)

instance Show Decl where
    show (Decl name typ) = name ++ ":" ++ show typ

isBasic :: Typ -> Bool
isBasic IntType    = True
isBasic DoubleType = True
isBasic BoolType   = True
isBasic _          = False


data Proc = Dot 
          | Proc {unProcGen :: String} 
            deriving (Eq, Ord)

instance Show Proc where
    show Dot = "<.>"
    show p = unProcGen p


instance Show Typ where
    show IntType       = "INTEGER"
    show (Sep c ps t)  = concat [ "separate <", show c, ">"
                                , show (map unProcGen ps)," ",show t
                                ]
    show DoubleType    = "REAL"
    show NoType        = "notype"
    show VoidType      = "NONE"
    show BoolType      = "BOOLEAN"
    show (Like e)      = "like " ++ show e
    show (ClassType s gs) = s ++ show gs
    show (TupleType typesDecls) = "TUPLE " ++ show typesDecls

type ClassName = String

classNameType :: Typ -> String
classNameType (ClassType cn _) = cn 
classNameType (Sep _ _ cn) = cn
classNameType _ = error "Non-class type"


type Stmt = PosAbsStmt Expr
type UnPosStmt = AbsStmt Expr
type PosAbsStmt a = Pos (AbsStmt a)
data AbsStmt a = Assign a a
               | AssignAttempt a a
               | If a (PosAbsStmt a) [ElseIfPart a] (Maybe (PosAbsStmt a))
               | Malloc ClassName
               | Create (Maybe Typ) a String [a]
               | Across a String (PosAbsStmt a)
               | Loop (PosAbsStmt a) [Clause a] a (PosAbsStmt a) (Maybe a) 
               | CallStmt a
               | Retry
               | Inspect a [([a], PosAbsStmt a)] (Maybe (PosAbsStmt a))
               | Check [Clause a]
               | CheckBlock [Clause a] (PosAbsStmt a)
               | Block [PosAbsStmt a]
               | Debug String (PosAbsStmt a)
               | Print a
               | PrintD a
               | BuiltIn deriving Eq

data ElseIfPart a = ElseIfPart a (PosAbsStmt a) deriving (Show, Eq)

instance Show a => Show (AbsStmt a) where
    show (Block ss) = intercalate ";\n" . map show $ ss
    show (If b body elseifs elseMb) = concat
        [ "if ", show b, "\n"
        , "then ", show body, "\n"
        , "elseifs: ", show elseifs, "\n"
        , "else: ", show elseMb
        ]
    show (Inspect i cases def) = "inspect " ++ show i 
        ++ concat (map showCase cases)
        ++ showDefault def
    show (Across e as stmt) = "across " ++ show e ++ " " ++ as ++ 
                              "\nloop\n" ++ show stmt ++ "\nend"
    show Retry = "retry"
    show (Check cs) = "check " ++ show cs ++ " end"
    show (CheckBlock e body) = "checkBlock " ++ show e ++ "\n" ++ show body
    show (Create t trg fName args) = 
        concat ["create ",braced t,show trg,".",fName,show args]
    show (CallStmt e) = show e
    show (Assign i e) = show i ++ " := " ++ show e ++ "\n"
    show (AssignAttempt i e) = show i ++ " ?= " ++ show e ++ "\n"
    show (Print e) = "Printing: " ++ show e ++ "\n"
    show (PrintD e) = "PrintingD: " ++ show e ++ "\n"
    show (Loop fr _ un l var) = "from" ++ show fr ++ " until" ++ show un ++
                          " loop " ++ show l ++ "variant" ++ show var ++ "end"
    show (Malloc s) = "Malloc: " ++ show s
    show (Debug str stmt) = "debug (" ++ str ++ ")\n" ++ show stmt ++ "end\n"
    show BuiltIn = "built_in"
  
braced t = case t of
  Nothing -> ""
  Just t' -> "{" ++ show t' ++ "}"
  
showCase (l, s) = "when " ++ show l ++ " then\n" ++ show s
showDefault Nothing = ""
showDefault (Just s) = "else\n" ++ show s


insertDecl :: Decl -> Map String Typ -> Map String Typ
insertDecl (Decl s t) = Map.insert s t

declsToMap :: [Decl] -> Map String Typ
declsToMap = foldr insertDecl Map.empty


data ProcExpr = LessThan Proc Proc deriving (Show, Eq)

data ProcDecl = SubTop Proc
              | CreateLessThan Proc Proc 
                deriving (Show, Eq)

newVar :: ProcDecl -> Proc
newVar (SubTop   p) = p
newVar (CreateLessThan p _) = p


data Clause a = Clause 
    { clauseName :: Maybe String
    , clauseExpr :: a
    } deriving (Show, Eq)


data Note = Note { noteTag :: String
                 , noteContent :: [UnPosExpr]
                 } deriving (Show, Eq)