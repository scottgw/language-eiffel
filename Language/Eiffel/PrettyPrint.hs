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
                genericsDoc (generics c) <+> procGenDoc (procGeneric c)
           , text ""
           , text "feature"
           , nest2 $ vcat $ punctuate newline $ map decl (attributes c)
           , nest2 $ vcat $ punctuate newline $ map featureDoc (features c)
           , text ""
           , invars (invnts c)
           , text "end"
           ]

angles d = langle <> d <> rangle
langle = char '<'
rangle = char '>'


procDoc (Proc s) = text s

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
featureBodyDoc ft = vcat [ locals ft
                         , text "do"
                         , nest2 $ stmt $ featureBody ft
                         ]

locals ft = text "local" $?$ nest2 (vcat $ map decl (featureLocal ft))

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
stmt' (Create t n es) = text "create" <+> expr' 0 (QualCall t n es)
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

expr = exprPrec 0

exprPrec :: Int -> Expr -> Doc
exprPrec i = expr' i . contents

expr' _ (UnqualCall n es) = text n <+> args es
expr' _ (QualCall t n es) = target <> text n <+> args es
    where 
      target = case contents t of
                 CurrentVar -> empty
                 _ -> expr t <> char '.'
expr' i (UnOpExpr uop e) = text (unop uop) <+> expr e
expr' i (BinOpExpr (SymbolOp op) e1 e2)
  | op == "[]" = exprPrec i e1 <+> brackets (expr e2)
  | otherwise =  condParens (i > 1) (exprPrec i e1 <+> text op <+> exprPrec (i + 1) e2)
expr' i (BinOpExpr bop e1 e2) = 
  condParens (i > p) 
             (exprPrec lp e1 <+> text op <+> exprPrec rp e2)
  where (op, p) = binop bop
        lp = p
        rp = p + 1
expr' _ (Attached t e asVar) = 
  text "attached" <+> braces (type' t) <+> expr e <+> text "as" <+> text asVar
expr' _ (VarOrCall s)     = text s
expr' _ ResultVar         = text "Result"
expr' _ CurrentVar        = text "Current"
expr' _ LitVoid           = text "Void"
expr' _ (LitChar c)       = quotes (char c)
expr' _ (LitInt i)        = int i
expr' _ (LitBool b)       = text (show b)
expr' _ (LitDouble d)     = double d
expr' _ (Cast t e)        = braces (type' t) <+> expr e
expr' _ (Tuple es)        = brackets (hcat $ punctuate comma (map expr es))
expr' _ (Agent e)         = text "agent" <+> expr e
expr' _ (InlineAgent ss)  = text "agent" <+> vcat (map stmt ss)
expr' _ s                 = error (show s)

condParens True  e = parens e
condParens False e = e

unop Not = "not"
unop Old = "old"

opList = [ (Add, ("+", 6))
         , (Sub, ("-", 6))
         , (Mul, ("*", 7))
         , (Div, ("/", 7))
         , (And, ("and", 5)) 
         , (Or,  ("or", 5))
         , (Implies, ("implies", 4))
         ]

binop :: BinOp -> (String, Int)
binop (SymbolOp o) = (o, 1)
binop (RelOp r _)  = (relop r, 3)
binop o = 
  case lookup o opList of
    Just (n,p) -> (n,p)
    Nothing -> error "binop: could not find operator"

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