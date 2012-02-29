{-# LANGUAGE KindSignatures #-}

module Language.Eiffel.Feature where

import Data.Map (Map)

import Language.Eiffel.Clause
import Language.Eiffel.Decl
import Language.Eiffel.Expr
import Language.Eiffel.Note
import Language.Eiffel.Stmt
import Language.Eiffel.Typ

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
    } deriving (Show, Eq)

data RoutineBody exp 
  = RoutineDefer
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
 


makeRoutineI :: AbsRoutine body exp -> RoutineI
makeRoutineI f = f { routineReq = Contract False []
                   , routineEns = Contract False []
                   , routineImpl = EmptyBody
                   }

argMap :: RoutineWithBody a -> Map String Typ
argMap = declsToMap . routineArgs

localMap :: RoutineWithBody a -> Map String Typ
localMap = declsToMap . routineLocal . routineImpl

updFeatBody :: RoutineBody a -> PosAbsStmt b -> RoutineBody b
updFeatBody impl body = impl {routineBody = body}
