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

data AbsFeature (body :: * -> *) exp = 
    AbsFeature 
    { 
      featureFroz   :: Bool,
      featureName   :: String,
      featureAlias  :: Maybe String,
      featureArgs   :: [Decl],
      featureResult :: Typ,
      featureAssigner :: Maybe String,
      featureNote   :: [Note],
      featureProcs  :: [Proc],
      featureReq    :: [Clause exp],
      featureReqLk  :: [ProcExpr],

      featureImpl   :: body exp,

      featureEns    :: [Clause exp],
      featureEnsLk  :: [Proc]
    } deriving (Show, Eq)

data FeatureBody exp 
  = FeatureDefer
  | FeatureBody 
    {
      featureLocal :: [Decl],
      featureLocalProcs :: [ProcDecl],
      featureBody  :: PosAbsStmt exp
    } deriving (Show, Eq)

makeFeatureI :: AbsFeature body exp -> FeatureI
makeFeatureI f = f {featureReq = [], featureEns = [], featureImpl = EmptyBody}

argMap :: FeatureWithBody a -> Map String Typ
argMap = declsToMap . featureArgs

localMap :: FeatureWithBody a -> Map String Typ
localMap = declsToMap . featureLocal . featureImpl

updFeatBody :: FeatureBody a -> PosAbsStmt b -> FeatureBody b
updFeatBody impl body = impl {featureBody = body}

{-
instance Show FeatureDecl where
    show (FeatureDecl name fr procEns args res pGens) =
        let 
            resStr = case res of
                       NoType -> ""
                       t      -> ":" ++ show t
            frz = if fr then "frozen" else ""
        in
          frz ++ 
          name ++ "(" ++ concat (intersperse "," (map show args)) ++ ")" ++ 
                  resStr ++ show pGens
-}