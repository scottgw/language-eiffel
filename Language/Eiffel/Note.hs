module Language.Eiffel.Note where

data Note = Note { noteTag :: String
                 , noteContent :: Either String [String]
                 } deriving (Show, Eq)