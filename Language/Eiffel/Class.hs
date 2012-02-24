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
      deferredClass :: Bool,
      classNote  :: [Note],
      className  :: ClassName,
      currProc   :: Proc,
      procGeneric :: [Proc],
      procExpr   :: [ProcDecl],
      generics   :: [Generic],
      inherit    :: [InheritClause],
      creates    :: [CreateClause],
	  converts   :: [ConvertClause],
      featureClauses   :: [FeatureClause body exp],
      invnts     :: [Clause exp]
    } deriving Show

data InheritClause 
    = InheritClause 
      { inheritClass :: Typ
      , redefine :: [String]
      , renames :: [RenameClause]
      } deriving Show
                 
data RenameClause = 
  Rename { renameOrig :: String
         , renameNew :: String
         , renameAlias :: Maybe String
         } deriving Show

data Generic = 
  Generic { genericName :: ClassName 
         , genericConstraints :: [Typ]
         } deriving Show 

data CreateClause = 
  CreateClause { createExportNames :: [ClassName]
         , createNames :: [String]
		 } deriving Show
		 
data ConvertClause = ConvertFrom String Typ | ConvertTo String Typ deriving Show

data FeatureClause body exp =
  FeatureClause { exportNames :: [ClassName]
                , features :: [AbsFeature body exp]
                , attributes :: [Attribute]
                , constants :: [Constant exp]
                } deriving Show

data Attribute = 
  Attribute { attrDecl :: Decl
            , attrAssign :: Maybe String
            , attrNotes :: [Note]
            } deriving Show
  
data Constant exp = 
  Constant { constDecl :: Decl
           , constVal :: exp
           } deriving Show  


constToAttr :: Constant exp -> Attribute
constToAttr (Constant d _) = Attribute d Nothing []

allAttributes = concatMap attributes . featureClauses
allFeatures = concatMap features . featureClauses
allConstants = concatMap constants . featureClauses
allCreates = concatMap createNames . creates

mapFeatures f clause = clause {features = map f (features clause)}
mapAttributes f clause = clause {attributes = map f (attributes clause)}
mapConstants f clause = clause {constants = map f (constants clause)}

mapExprs featrF constF clause = 
  clause { features = map featrF (features clause)
         , constants = map constF (constants clause)
         }

classMapAttributes f c = 
  c {featureClauses = map (mapAttributes f) (featureClauses c)}

classMapFeatures :: (AbsFeature body exp -> AbsFeature body exp) 
            -> AbsClas body exp -> AbsClas body exp
classMapFeatures f c = 
  c {featureClauses = map (mapFeatures f) (featureClauses c)}



classMapExprs :: (AbsFeature body exp -> AbsFeature body' exp') 
                 -> (Clause exp -> Clause exp')
                 -> (Constant exp -> Constant exp')
                 -> AbsClas body exp -> AbsClas body' exp'
classMapExprs featrF clauseF constF c = 
  c { featureClauses = map (mapExprs featrF constF) (featureClauses c)
    , invnts         = map clauseF (invnts c)
    }


makeFeatureIs clause =
  clause { features = map makeFeatureI (features clause)
         , attributes = attributes clause ++ 
                        map constToAttr (constants clause)
         , constants = []
         }

clasInterface :: AbsClas body exp -> ClasInterface
clasInterface c = 
  c { featureClauses = map makeFeatureIs (featureClauses c)
    , invnts = []}

clasMap :: [AbsClas body exp] -> Map ClassName (AbsClas body exp)
clasMap = Map.fromList . map (\ c -> (className c, c))

attrMap :: AbsClas body exp -> Map String Typ
attrMap = declsToMap . map attrDecl . allAttributes

findFeature :: Clas -> String -> Maybe Feature
findFeature c fName =
    let fs = allFeatures c
        ffs = filter ( (== fName) . featureName) fs
    in listToMaybe ffs

findOperator :: ClasInterface -> String -> Maybe FeatureI
findOperator c opName =
    let fs = allFeatures c
        ffs = filter ( (== Just opName) . featureAlias) fs
    in listToMaybe ffs

findFeatureInt :: ClasInterface -> String -> Maybe FeatureI
findFeatureInt c fName =
    let fs = allFeatures c
        ffs = filter ( (== fName) . featureName) fs
    in listToMaybe ffs

findAttrInt :: ClasInterface -> String -> Maybe Decl
findAttrInt c attrName = 
    let as = map attrDecl (allAttributes c)
        as' = filter ( ( == attrName) . declName) as
    in listToMaybe as'

updFeatureClauses :: AbsClas body exp -> [FeatureClause body exp] 
                     -> AbsClas body exp
updFeatureClauses c fcs = c {featureClauses = fcs}

fullName :: AbsClas body exp -> FeatureI -> String
fullName c f = fullNameStr (className c) (featureName f)

fullNameStr :: String -> String -> String
fullNameStr = (++)

genericStubs :: AbsClas body exp -> [AbsClas body exp]
genericStubs = map makeGenericStub . generics

-- for the G,H in something like `class A [G,H]'
makeGenericStub :: Generic -> AbsClas body exp
makeGenericStub (Generic g _) = AbsClas 
                  { deferredClass = False
                  , classNote  = []
                  , className  = g
                  , currProc   = Dot
                  , procGeneric = []
                  , procExpr   = []
                  , generics   = []
                  , inherit    = []
                  , creates    = []
				  , converts   = []
                  , featureClauses   = []
                  , invnts     = []
                  }
