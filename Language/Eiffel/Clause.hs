module Language.Eiffel.Clause where

data Clause a = Clause 
    { clauseName :: Maybe String
    , clauseExpr :: a
    } deriving (Show, Eq)