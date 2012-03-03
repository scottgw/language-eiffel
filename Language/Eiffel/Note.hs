module Language.Eiffel.Note where

import Language.Eiffel.Expr

data Note = Note { noteTag :: String
                 , noteContent :: [UnPosExpr]
                 } deriving (Show, Eq)