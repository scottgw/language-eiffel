{-# LANGUAGE KindSignatures #-}

module Language.Eiffel.Feature where

import Data.Map (Map)

import Language.Eiffel.Clause
import Language.Eiffel.Decl
import Language.Eiffel.Expr
import Language.Eiffel.Note
import Language.Eiffel.Stmt
import Language.Eiffel.Typ

type FeatureI = AbsFeature EmptyBody Expr
type FeatureWithBody exp = AbsFeature FeatureBody exp
type Feature = FeatureWithBody Expr

data EmptyBody exp = EmptyBody deriving (Show, Eq)

data Contract exp = 
  Contract { contractInherited :: Bool 
           , contractClauses :: [Clause exp]
           } deriving (Show, Eq)

data AbsFeature (body :: * -> *) exp = 
    AbsFeature 
    { featureFroz   :: Bool
    , featureName   :: String
    , featureAlias  :: Maybe String
    , featureArgs   :: [Decl]
    , featureResult :: Typ
    , featureAssigner :: Maybe String
    , featureNote   :: [Note]
    , featureProcs  :: [Proc]
    , featureReq    :: Contract exp
    , featureReqLk  :: [ProcExpr]
    , featureImpl   :: body exp
    , featureEns    :: Contract exp
    , featureEnsLk  :: [Proc]
    } deriving (Show, Eq)

data FeatureBody exp 
  = FeatureDefer
  | FeatureBody 
    { featureLocal :: [Decl]
    , featureLocalProcs :: [ProcDecl]
    , featureBody  :: PosAbsStmt exp
    } deriving (Show, Eq)

makeFeatureI :: AbsFeature body exp -> FeatureI
makeFeatureI f = f { featureReq = Contract False []
                   , featureEns = Contract False []
                   , featureImpl = EmptyBody
                   }

argMap :: FeatureWithBody a -> Map String Typ
argMap = declsToMap . featureArgs

localMap :: FeatureWithBody a -> Map String Typ
localMap = declsToMap . featureLocal . featureImpl

updFeatBody :: FeatureBody a -> PosAbsStmt b -> FeatureBody b
updFeatBody impl body = impl {featureBody = body}
