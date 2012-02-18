module Language.Eiffel.Stmt where

import Language.Eiffel.Position
import {-# SOURCE #-} Language.Eiffel.Expr

type Stmt = PosAbsStmt Expr
type UnPosStmt = AbsStmt Expr
type PosAbsStmt a = Pos (AbsStmt a)

data AbsStmt a

instance Eq a => Eq (AbsStmt a)

instance Show a => Show (AbsStmt a)