{-# LANGUAGE TemplateHaskell #-}
module Language.Eiffel.Syntax where

import Data.List

import Data.DeriveTH
import Data.Binary

import Language.Eiffel.Position

type Clas = ClasBody Expr
type ClasBody exp = AbsClas (RoutineBody exp) exp
type ClasInterface = AbsClas EmptyBody Expr
type ClasI exp = AbsClas (RoutineBody exp) exp

data AbsClas body exp =
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

type RoutineI = AbsRoutine EmptyBody Expr
type RoutineWithBody exp = AbsRoutine (RoutineBody exp) exp
type Routine = RoutineWithBody Expr

data EmptyBody = EmptyBody deriving (Show, Eq)

data Contract exp = 
  Contract { contractInherited :: Bool 
           , contractClauses :: [Clause exp]
           } deriving (Show, Eq)

data AbsRoutine body exp = 
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
    , routineImpl   :: body
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
             deriving (Show, Ord, Eq)

data ROp = Lte
         | Lt 
         | Eq 
         | TildeEq
         | Neq
         | TildeNeq
         | Gt 
         | Gte
           deriving (Show, Ord, Eq)

data UnOp = Not
          | Neg
          | Old
            deriving (Show, Ord, Eq)

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
  | LitInt Integer
  | LitBool Bool
  | LitVoid
  | LitDouble Double 
  | LitType Typ deriving (Ord, Eq)

data Quant = All | Some deriving (Eq, Ord, Show)

commaSepShow es = intercalate "," (map show es)
argsShow args = "(" ++ commaSepShow args ++ ")"
defaultCreate = "default_create"

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
    show (AcrossExpr c as quant e) = 
      "across " ++ show c ++ " as " ++ as ++ " " ++ show quant ++ " " ++ show e
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
         | TupleType (Either [Typ] [Decl])
         | Sep (Maybe Proc) [Proc] String
         | Like String
         | VoidType
         | NoType deriving (Eq, Ord)

data Decl = Decl 
    { declName :: String,
      declType :: Typ
    } deriving (Ord, Eq)

instance Show Decl where
    show (Decl name typ) = name ++ ":" ++ show typ


data Proc = Dot 
          | Proc {unProcGen :: String} 
            deriving (Eq, Ord)

instance Show Proc where
    show Dot = "<.>"
    show p = unProcGen p


instance Show Typ where
    show (Sep c ps t)  = concat [ "separate <", show c, ">"
                                , show (map unProcGen ps)," ",show t
                                ]
    show NoType        = "notype"
    show VoidType      = "NONE"
    show (Like e)      = "like " ++ show e
    show (ClassType s gs) = s ++ show gs
    show (TupleType typesDecls) = "TUPLE " ++ show typesDecls

type ClassName = String

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
               | BuiltIn deriving (Ord, Eq)

data ElseIfPart a = ElseIfPart a (PosAbsStmt a) deriving (Show, Ord, Eq)

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

data ProcExpr = LessThan Proc Proc deriving (Show, Eq)

data ProcDecl = SubTop Proc
              | CreateLessThan Proc Proc 
                deriving (Show, Eq)

data Clause a = Clause 
    { clauseName :: Maybe String
    , clauseExpr :: a
    } deriving (Show, Ord, Eq)


data Note = Note { noteTag :: String
                 , noteContent :: [UnPosExpr]
                 } deriving (Show, Eq)


$( derive makeBinary ''Typ )
$( derive makeBinary ''UnPosExpr )
$( derive makeBinary ''BinOp )
$( derive makeBinary ''Quant )
$( derive makeBinary ''Decl )
$( derive makeBinary ''UnOp )
$( derive makeBinary ''ROp )

$( derive makeBinary ''AbsStmt )
$( derive makeBinary ''ElseIfPart )

$( derive makeBinary ''ProcExpr )

$( derive makeBinary ''ExportList )
$( derive makeBinary ''ExportClause )
$( derive makeBinary ''RenameClause )

$( derive makeBinary ''Constant )
$( derive makeBinary ''Attribute )
$( derive makeBinary ''AbsRoutine )
$( derive makeBinary ''EmptyBody )

$( derive makeBinary ''Contract )

$( derive makeBinary ''Proc )
$( derive makeBinary ''ProcDecl )
$( derive makeBinary ''Generic )
$( derive makeBinary ''Clause )
$( derive makeBinary ''FeatureClause )
$( derive makeBinary ''ConvertClause )
$( derive makeBinary ''CreateClause )
$( derive makeBinary ''InheritClause )
$( derive makeBinary ''Inheritance )
$( derive makeBinary ''Note )
$( derive makeBinary ''AbsClas )
