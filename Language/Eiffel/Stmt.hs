{-# LANGUAGE TypeSynonymInstances #-}

module Language.Eiffel.Stmt where

import Data.List (intercalate)

import Language.Eiffel.Typ
import Language.Eiffel.Expr
import Language.Eiffel.Position

type Stmt = PosAbsStmt Expr
type UnPosStmt = AbsStmt Expr
type PosAbsStmt a = Pos (AbsStmt a)
data AbsStmt a = Assign a a
               | If a (PosAbsStmt a) (PosAbsStmt a)
               | Malloc ClassName
               | Create a String [a]
               | DefCreate a
               | Loop (PosAbsStmt a) a (PosAbsStmt a)
               | CallStmt a
               | Block [PosAbsStmt a]
               | Print a
               | PrintD a
               | BuiltIn

instance Show a => Show (AbsStmt a) where
    show (Block ss) = intercalate ";\n" . map show $ ss
    show (If b s1 s2) = concat
        [
         "if ", show b, "\n",
         "then ", show s1, "\n",
         "else ", show s2, "\n"
        ]
    show (Create trg fName args) = 
        concat ["create ",show trg,".",fName,show args]
    show (DefCreate e) = "create(def) " ++ show e
    show (CallStmt e) = show e
    show (Assign i e) = show i ++ " := " ++ show e ++ "\n"
    show (Print e) = "Printing: " ++ show e ++ "\n"
    show (PrintD e) = "PrintingD: " ++ show e ++ "\n"
    show (Loop fr un l) = "from " ++ show fr ++ " until " ++ show un ++
                          " loop " ++ show l ++ "end\n"
    show (Malloc s) = "Malloc: " ++ show s
    show BuiltIn = "built_in"