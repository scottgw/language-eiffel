module Language.Eiffel.PrettyPrint where

import Data.Char
import Data.List

import Text.PrettyPrint

import Language.Eiffel.Class
import Language.Eiffel.Clause
import Language.Eiffel.Expr
import Language.Eiffel.Decl
import Language.Eiffel.Feature
import Language.Eiffel.Note
import Language.Eiffel.Stmt
import Language.Eiffel.Typ
import Language.Eiffel.Position

newline = char '\n'

ups = map toUpper
toDoc c 
    = vcat [ notes (classNote c)
           , text "class" <+> text (ups $ className c) <+> 
                genericsDoc (generics c) <+> procGenDoc (procGeneric c) <> newline
           , text "feature"
           , nest2 $ vcat $ punctuate newline $ map decl (attributes c)
           , nest2 $ vcat $ punctuate newline $ map featureDoc (features c)
           , invars (invnts c)
           , text "end"
           ]

angles d = langle <> d <> rangle
-- brackets d = lbracket <> d <> rbracket
procDoc (Proc s) = text s
langle = char '<'
rangle = char '>'
lbracket = char '['
rbracket = char ']'

genericsDoc [] = empty
genericsDoc gs = brackets (hcat $ map go gs)
  where go (Generic g) = text g

notes [] = empty
notes ns = vcat [ text "note"
                , nest2 (vcat $ map note ns)
                ]
  where note (Note tag content) = text tag <> colon <+> printEither content
        printEither (Left s)    = doubleQuotes $ text s
        printEither (Right ids) = hcat $ punctuate comma (map text ids)

invars is = text "invariant" $?$ clausesDoc is
                 

procGenDoc [] = empty
procGenDoc ps = go ps
  where go = angles . hsep . punctuate comma . map procDoc

decl :: Decl -> Doc
decl (Decl label typ) = text label <> typeDoc typ

typeDoc NoType = empty
typeDoc t = text ":" <+> type' t

type' :: Typ -> Doc
type' (ClassType str gens) = text (ups str) <+> genDoc gens
type' IntType    = text "INTEGER"
type' DoubleType = text "REAL"
type' BoolType   = text "BOOLEAN"
type' VoidType   = text "NONE"
type' (Like s)   = text "like" <+> text s
type' NoType     = empty
type' (Sep mP ps str) = sepDoc <+> procM mP <+> procs ps <+> text str

featureDoc :: Feature -> Doc
featureDoc f 
    = (text (featureName f) <+> formArgs (featureArgs f) <> 
            typeDoc (featureResult f) <+>
            procs (featureProcs f)) $+$
        (nest2 $ vcat 
           [ notes (featureNote f)
           , text "require" $?$ clausesDoc (featureReq f) 
           , text "require-order" $?$   nest2 (procExprs f)
           , text "lock" $?$ nest2 (locks (featureEnsLk f))
           , featureBodyDoc $ featureImpl f
           , text "ensure" $?$ clausesDoc (featureEns f)
           , text "end"
           ]
        )

featureBodyDoc FeatureDefer = text "deferred"
featureBodyDoc ft = vcat [ text "do"
                         , nest2 $ stmt $ featureBody ft
                         ]

procExprs = vcat . punctuate comma . map procExprD . featureReqLk

($?$) :: Doc -> Doc -> Doc
($?$) l e 
    | isEmpty e = empty
    | otherwise = l $+$ e

clausesDoc :: [Clause Expr] -> Doc
clausesDoc cs = nest2 (vcat $ map clause cs)

clause :: Clause Expr -> Doc
clause (Clause n e) = text n <> colon <+> expr e

nest2 = nest 2

stmt = stmt' . contents

stmt' (Assign l e) = expr l <+> text ":=" <+> expr e
stmt' (CallStmt e) = expr e
stmt' (If e s1 s2) = 
    vcat [ text "if" <+> expr e <+> text "then"
         , nest2 (stmt s1)
         , text "else"
         , nest2 (stmt s2)
         , text "end"
         ]
stmt' (BuiltIn)  = text "builtin"
stmt' (Create t n es) = text "create" <+> expr' (QualCall t n es)
stmt'  (DefCreate v) = text "create" <+> expr v
stmt' (Block ss) = vcat (map stmt ss)
stmt' (Check cs) = vcat [ text "check"
                        , nest2 (vcat (map clause cs))
                        , text "end"
                        ]
stmt' (Loop from until loop) = 
  vcat [ text "from"
       , nest2 (stmt from)
       , text "until"
       , nest2 (expr until)
       , text "loop"
       , nest2 (stmt loop)
       , text "end"
       ]
stmt' s = error (show s)

expr = expr' . contents

expr' (UnqualCall n es) = text n <+> args es
expr' (QualCall t n es) = target <> text n <+> args es
    where 
      target = case contents t of
                 CurrentVar -> empty
                 _ -> expr t <> char '.'
expr' (UnOpExpr uop e) = text (unop uop) <+> expr e
expr' (BinOpExpr (SymbolOp op) e1 e2)
  | op == "[]" = parens $ expr e1 <+> brackets (expr e2)
  | otherwise =  parens $ expr e1 <+> text op <+> expr e2
expr' (BinOpExpr bop e1 e2) = parens $ expr e1 <+> text (binop bop) <+> expr e2
expr' (Attached t e asVar) = 
  text "attached" <+> braces (type' t) <+> expr e <+> text "as" <+> text asVar
expr' (VarOrCall s)     = text s
expr' ResultVar         = text "Result"
expr' CurrentVar        = text "Current"
expr' LitVoid           = text "Void"
expr' (LitChar c)       = quotes (char c)
expr' (LitInt i)        = int i
expr' (LitBool b)       = text (show b)
expr' (LitDouble d)     = double d
expr' (Cast t e)        = braces (type' t) <+> expr e
expr' (Tuple es)        = brackets (hcat $ punctuate comma (map expr es))
expr' (Agent e)         = text "agent" <+> expr e
expr' (InlineAgent ss)  = text "agent" <+> vcat (map stmt ss)
expr' s                 = error (show s)

unop Not = "not"
unop Old = "old"

binop Add = "+"
binop Sub = "-"
binop Mul = "*"
binop Div = "/"
binop And = "and"
binop Or  = "or"
binop Implies = "implies"
binop (RelOp r _) = relop r


relop Lt  = "<"
relop Lte = "<="
relop Gt  = ">"
relop Gte = ">="
relop Eq  = "="
relop Neq = "/="
relop TildeEq = "~"

args [] = empty
args es = parens $ hsep $ punctuate comma (map expr es)

formArgs [] = empty
formArgs ds = parens $ hsep $ punctuate semi (map decl ds) 

genDoc :: [Typ] -> Doc
genDoc [] = empty
genDoc ps = brackets $ hcat $ punctuate comma (map type' ps)

procExprD (LessThan a b) = proc a <+> langle <+> proc b
locks [] = empty
locks ps = hsep $ punctuate comma (map proc ps)

procs [] = empty
procs ps = angles $ locks ps
proc (Proc p) = text p
proc Dot      = text "dot"
procM = maybe empty (angles . proc)
sepDoc = text "separate"