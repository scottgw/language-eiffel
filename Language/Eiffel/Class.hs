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
    } deriving (Eq, Show)

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
         , genericConstraints :: [Typ]
         } deriving (Show, Eq) 

data CreateClause = 
  CreateClause { createExportNames :: [ClassName]
               , createNames :: [String]
               } deriving (Show, Eq)
		 
data ConvertClause = ConvertFrom String Typ | ConvertTo String Typ deriving (Show, Eq)

data FeatureClause body exp =
  FeatureClause { exportNames :: [ClassName]
                , features :: [AbsFeature body exp]
                , attributes :: [Attribute exp]
                , constants :: [Constant exp]
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


constToAttr :: Constant exp -> Attribute Expr
constToAttr (Constant froz d _) = Attribute froz d Nothing [] (Contract False []) (Contract False [])

allAttributes = concatMap attributes . featureClauses
allFeatures = concatMap features . featureClauses
allConstants = concatMap constants . featureClauses
allCreates = concatMap createNames . creates
allAttributeDecls = map attrDecl . allAttributes
allConstantDecls = map constDecl . allConstants

isCreateName n c = n `elem` allCreates c

mapFeatures f clause = clause {features = map f (features clause)}
mapAttributes f clause = clause {attributes = map f (attributes clause)}
mapConstants f clause = clause {constants = map f (constants clause)}

mapExprs featrF constF clauseF fClause = 
  fClause { features = map featrF (features fClause)
          , constants = map constF (constants fClause)
          , attributes = map (\a -> a { attrEns = (attrEns a) { contractClauses = map clauseF ((contractClauses . attrEns) a) }
                                      , attrReq = (attrReq a) { contractClauses = map clauseF ((contractClauses . attrReq) a) }
                                      }
                             ) (attributes fClause)
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
  c { featureClauses = map (mapExprs featrF constF clauseF) (featureClauses c)
    , invnts         = map clauseF (invnts c)
    }


makeFeatureIs :: FeatureClause body exp -> FeatureClause EmptyBody Expr
makeFeatureIs clause =
  clause { features   = map makeFeatureI (features clause)
         , attributes = map makeAttributeI (attributes clause) ++ 
                        map constToAttr (constants clause)
         , constants  = []
         }

makeAttributeI :: Attribute exp -> Attribute Expr
makeAttributeI (Attribute froz decl assgn notes _ _) =
  Attribute froz decl assgn notes (Contract False []) (Contract False [])

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
    let ats = filter ( ( == attrName) . declName) (allAttributeDecls c)
    in listToMaybe ats
    
findConstantInt :: ClasInterface -> String -> Maybe Decl
findConstantInt c constName = 
    let cts = filter ( ( == constName) . declName) (allConstantDecls c)
    in listToMaybe cts    

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
