{-# LANGUAGE TypeSynonymInstances #-}

module Language.Eiffel.Stmt where

import Data.List (intercalate)

import Language.Eiffel.Clause
import Language.Eiffel.Typ
import Language.Eiffel.Expr
import Language.Eiffel.Position

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
               | Inspect a [([a], PosAbsStmt a)] (Maybe (PosAbsStmt a))
               | Check [Clause a]
               | CheckBlock a (PosAbsStmt a)
               | Block [PosAbsStmt a]
               | Debug String (PosAbsStmt a)
               | Print a
               | PrintD a
               | BuiltIn deriving Eq

data ElseIfPart a = ElseIfPart a (PosAbsStmt a) deriving (Show, Eq)

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