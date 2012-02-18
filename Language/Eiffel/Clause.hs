module Language.Eiffel.Clause where

data Clause a = Clause 
    { clauseName :: String
    , clauseExpr :: a
    } deriving (Show, Eq)