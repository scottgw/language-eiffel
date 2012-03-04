module Language.Eiffel.Typ where

data Typ = ClassType ClassName [Typ]
         | IntType
         | TupleType (Either [Typ] [Decl])
         | Sep (Maybe Proc) [Proc] String
         | Like String
         | DoubleType
         | VoidType
         | NoType
         | BoolType deriving (Eq, Ord)

data Decl = Decl 
    { declName :: String,
      declType :: Typ
    } deriving (Ord, Eq)

instance Show Decl where
    show (Decl name typ) = name ++ ":" ++ show typ

isBasic :: Typ -> Bool
isBasic IntType    = True
isBasic DoubleType = True
isBasic BoolType   = True
isBasic _          = False


data Proc = Dot 
          | Proc {unProcGen :: String} 
            deriving (Eq, Ord)

instance Show Proc where
    show Dot = "<.>"
    show p = unProcGen p


instance Show Typ where
    show IntType       = "INTEGER"
    show (Sep c ps t)  = concat [ "separate <", show c, ">"
                                , show (map unProcGen ps)," ",show t
                                ]
    show DoubleType    = "REAL"
    show NoType        = "notype"
    show VoidType      = "NONE"
    show BoolType      = "BOOLEAN"
    show (Like e)      = "like " ++ show e
    show (ClassType s gs) = s ++ show gs
    show (TupleType typesDecls) = "TUPLE " ++ show typesDecls

type ClassName = String

classNameType :: Typ -> String
classNameType (ClassType cn _) = cn 
classNameType (Sep _ _ cn) = cn
classNameType _ = error "Non-class type"
