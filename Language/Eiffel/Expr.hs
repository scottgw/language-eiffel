module Language.Eiffel.Expr where

import Data.List

import Language.Eiffel.Typ
import Language.Eiffel.Decl
import Language.Eiffel.Position
import {-# SOURCE #-} Language.Eiffel.Stmt

type Expr = Pos UnPosExpr 

data BinOp = Add
           | Sub
           | Mul
           | Div
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
  | PrecursorCall (Maybe String) [Expr]
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  | Attached (Maybe Typ) Expr (Maybe String)
  | Agent Expr
  | Tuple [Expr]
  | InlineAgent [Decl] (Maybe Typ) [Stmt] [Expr]
  | TypedVar String Typ
  | VarOrCall String
  | ResultVar
  | CurrentVar
  | Cast Typ Expr
  | LitStaticClass Typ
  | LitString String
  | LitChar Char
  | LitInt Int
  | LitBool Bool
  | LitVoid
  | LitDouble Double 
  | LitType Typ deriving Eq

instance Show UnPosExpr where
    show (UnqualCall s args) 
        = s ++ "(" ++ intercalate "," (map show args) ++ ")"
    show (QualCall t s args)
        = show t ++ "." ++ s ++ "(" ++ intercalate "," (map show args) ++ ")"
    show (BinOpExpr op e1 e2) 
        = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (UnOpExpr op e) = "(" ++ show op ++ " " ++ show e ++ ")"
    show (Attached s1 e s2) = "(attached " ++ show s1 ++ ", " ++ show e ++ " as " ++ show s2 ++ ")"
    show (TypedVar var t) = "(" ++ var ++ ": " ++ show t ++ ")"
    show (VarOrCall s) = s
    show ResultVar  = "Result"
    show CurrentVar = "Current"
    show (Cast t e)    = "{" ++ show t ++ "}" ++ show e
    show (LitString s) = "\"" ++ s ++ "\""
    show (LitChar c) = "'" ++ [c] ++ "'"
    show (LitInt i)  = show i
    show (LitBool b) = show b
    show (LitDouble d) = show d
    show (LitStaticClass t) = "{" ++ show t ++ "}"
    show (LitType t) = "({" ++ show t ++ "})"
    show (Tuple es) = show es
    show (Agent e)  = "agent " ++ show e
    show (InlineAgent ds r ss args) = 
      "agent " ++ show ds ++ ":" ++ show r ++ " " ++ show ss ++ " " ++ show args
    show LitVoid = "Void"


