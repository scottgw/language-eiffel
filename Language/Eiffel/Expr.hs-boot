module Language.Eiffel.Expr where

import Language.Eiffel.Position

type Expr = Pos UnPosExpr 

data UnPosExpr

instance Eq UnPosExpr