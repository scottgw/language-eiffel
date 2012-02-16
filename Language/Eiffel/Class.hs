{-# LANGUAGE KindSignatures #-}
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
type ClasBody exp = AbsClas FeatureBody exp
type ClasInterface = AbsClas EmptyBody Expr
type ClasI exp = AbsClas FeatureBody exp

data AbsClas (body :: * -> *) exp =
    AbsClas
    {
      classNote  :: [Note],
      className  :: ClassName,
      currProc   :: Proc,
      procGeneric :: [Proc],
      procExpr   :: [ProcDecl],
      generics   :: [Generic],
      inherit    :: [InheritClause],
      creates    :: [String],
      attributes :: [Decl],
      features   :: [AbsFeature body exp],
      invnts     :: [Clause exp]
    } deriving Show

data InheritClause 
    = InheritClause 
      { inheritClass :: Typ
      , redefine :: [String]
      } deriving Show

data Generic = Generic ClassName deriving Show 

mapFeatures :: (AbsFeature body exp -> AbsFeature body exp) 
            -> AbsClas body exp -> AbsClas body exp
mapFeatures f c = c {features = map f (features c)}

clasInterface :: AbsClas body exp -> ClasInterface
clasInterface c = c {features = map makeFeatureI (features c), invnts = []}

clasMap :: [AbsClas body exp] -> Map ClassName (AbsClas body exp)
clasMap = Map.fromList . map (\ c -> (className c, c))

attrMap :: AbsClas body exp -> Map String Typ
attrMap = declsToMap . attributes

findFeature :: Clas -> String -> Maybe Feature
findFeature c fName =
    let fs = features c
        ffs = filter ( (== fName) . featureName) fs
    in listToMaybe ffs

findOperator :: ClasInterface -> String -> Maybe FeatureI
findOperator c opName =
    let fs = features c
        ffs = filter ( (== Just opName) . featureAlias) fs
    in listToMaybe ffs

findFeatureInt :: ClasInterface -> String -> Maybe FeatureI
findFeatureInt c fName =
    let fs = features c
        ffs = filter ( (== fName) . featureName) fs
    in listToMaybe ffs

findAttrInt :: ClasInterface -> String -> Maybe Decl
findAttrInt c attrName = 
    let as = attributes c
        as' = filter ( ( == attrName) . declName) as
    in listToMaybe as'

updFeatures :: AbsClas body exp -> [AbsFeature body exp] -> AbsClas body exp
updFeatures c fs = c {features = fs}

fullName :: AbsClas body exp -> FeatureI -> String
fullName c f = fullNameStr (className c) (featureName f)

fullNameStr :: String -> String -> String
fullNameStr = (++)

genericName :: Generic -> String
genericName (Generic cn) = cn

genericStubs :: AbsClas body exp -> [AbsClas body exp]
genericStubs = map makeGenericStub . generics

-- for the G,H in something like `class A [G,H]'
makeGenericStub :: Generic -> AbsClas body exp
makeGenericStub (Generic g) = AbsClas 
                  { classNote  = []
                  , className  = g
                  , currProc   = Dot
                  , procGeneric = []
                  , procExpr   = []
                  , generics   = []
                  , inherit    = []
                  , creates    = []
                  , attributes = []
                  , features   = []
                  , invnts     = []
                  }
