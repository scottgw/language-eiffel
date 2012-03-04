module Language.Eiffel.Decl where

import qualified Data.Map as Map
import Data.Map (Map)

import Language.Eiffel.Typ

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

