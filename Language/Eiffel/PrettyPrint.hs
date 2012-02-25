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
toDoc c =
  let defer = if deferredClass c then text "deferred" else empty
  in vcat [ notes (classNote c)
          , defer <+> text "class" <+> text (ups $ className c) <+> 
            genericsDoc (generics c) <+> procGenDoc (procGeneric c)
          , text ""
          , inheritClauses (inherit c)
          , vcat (map createClause (creates c))
          , convertClause (converts c)
          , vcat (map featureClause (featureClauses c))
          , text ""
          , invars (invnts c)
          , text "end"
          ]

inheritClauses cs =
  text "inherit" $?$ nest2 (vcat (map inheritClause cs))

inheritClause (InheritClause cls redefs renames) = 
  let renameDoc (Rename orig new alias) =
        text orig <+> text "as" <+> text new <+> 
          maybe empty (doubleQuotes . text) alias
  in vcat [ type' cls
          , text "rename" $?$ nest2 (vcat (map renameDoc renames))
          , text "redefine" $?$ nest2 (vcat (map text redefs))
          , if null redefs && null renames
            then empty else text "end"
          ] 
      
createClause (CreateClause exports names) = 
  let exps = if null exports 
             then empty 
             else  braces (commaSep (map text exports))
  in (text "create" <+> exps) $$ commaSep (map text names) 
  
convertClause []    = empty  
convertClause convs =
  let go (ConvertFrom fname t) = text fname <+> parens (braces (type' t))
      go (ConvertTo fname t) = text fname <> colon <+> braces (type' t)
  in text "convert" $$ vcat (map go convs)

featureClause (FeatureClause exports featrs attrs consts) = 
  let exps = if null exports 
             then empty 
             else  braces (commaSep (map text exports))
  in vcat [ text "feature" <+> exps
          , nest2 $ vcat $ punctuate newline $ map featureDoc featrs
          , nest2 $ vcat $ punctuate newline $ map attrDoc attrs
          , nest2 $ vcat $ punctuate newline $ map constDoc consts
          ]


commaSep = hcat . punctuate comma
angles d = langle <> d <> rangle
langle = char '<'
rangle = char '>'
squareQuotes t = vcat [ text "\"["
                      , t
                      , text "]\""
                      ]
                      
anyStringLiteral s = case s of
  '\n':_ -> squareQuotes $ text s
  _      -> doubleQuotes $ text s


procDoc (Proc s) = text s

genericsDoc [] = empty
genericsDoc gs = brackets (commaSep (map go gs))
  where go (Generic name constr) = text name <+> constraints constr
        constraints []           = empty
        constraints [t]          = text "->" <+> type' t
        constraints ts           = text "->" <+> braces (commaSep (map type' ts))

notes [] = empty
notes ns = vcat [ text "note"
                , nest2 (vcat $ map note ns)
                ]
  where note (Note tag content) = text tag <> colon <+> printEither content
        printEither (Left s)    = anyStringLiteral s
        printEither (Right ids) = commaSep (map text ids)

invars is = text "invariant" $?$ clausesDoc is
                 

procGenDoc [] = empty
procGenDoc ps = go ps
  where go = angles . hsep . punctuate comma . map procDoc

decl :: Decl -> Doc
decl (Decl label typ) = text label <> typeDoc typ

typeDoc NoType = empty
typeDoc t = text ":" <+> type' t

constDoc :: Constant Expr -> Doc
constDoc (Constant d val) = decl d <+> text "=" <+> expr val

attrDoc :: Attribute -> Doc
attrDoc (Attribute d assn ns) = decl d <+> assignText assn $$ noteText ns
  where assignText Nothing  = empty
        assignText (Just a) = text "assign" <+> text a
        noteText []        = empty
        noteText ns        = notes ns $$ text "attribute" $$ text "end"  

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
    = let header = text (featureName f) <+>
                   alias <+>
                   formArgs (featureArgs f) <> 
                   typeDoc (featureResult f) <+>
                   procs (featureProcs f)
          alias = 
            case featureAlias f of
              Nothing   -> empty
              Just name -> text "alias" <+> doubleQuotes (text name)
          assign =
            case featureAssigner f of
              Nothing -> empty
              Just name -> text "assign" <+> text name
      in header $+$ 
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
clause (Clause nameMb e) = maybe empty text nameMb <> colon <+> expr e

nest2 = nest 2

stmt = stmt' . contents

stmt' (Assign l e) = expr l <+> text ":=" <+> expr e
stmt' (CallStmt e) = expr e
stmt' (If e s1 s2) = 
  let elsePart = case contents s2 of
                  Block [] -> empty
                  _        -> vcat [text "else", nest2 (stmt s2)]
  in vcat [ text "if" <+> expr e <+> text "then"
          , nest2 (stmt s1)
          , elsePart
          , text "end"
          ]
stmt' (Inspect e whens elseMb) =
  let elsePart = case elseMb of
        Nothing -> empty
        Just s -> text "else" $+$ nest2 (stmt s)
      whenParts (e,s) = (text "when" <+> expr e <+> text "then") $+$ 
                        nest2 (stmt s)
  in vcat [ text "inspect" <+> expr e
          , vcat (map whenParts whens)
          , elsePart
          , text "end"
          ]
stmt' (BuiltIn)  = text "builtin"
stmt' (Create t tar n es) = text "create" <+> maybe empty (braces . type') t <+> expr' 0 (QualCall tar n es)
stmt' (DefCreate t v) = text "create" <+> maybe empty (braces . type') t <+> expr v
stmt' (Block ss) = vcat (map stmt ss)
stmt' (Check cs) = vcat [ text "check"
                        , nest2 (vcat (map clause cs))
                        , text "end"
                        ]
stmt' (Loop from invs until loop) = 
  vcat [ text "from"
       , nest2 (stmt from)
       , text "invariant" $?$ clausesDoc invs
       , text "until"
       , nest2 (expr until)
       , text "loop"
       , nest2 (stmt loop)
       , text "end"
       ]
stmt' (Debug str body) = text "debug" <+> if null str then empty else (parens . anyStringLiteral) str $$ nest2 (stmt body)
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
expr' _ (PrecursorCall cname es) = text "Precursor" <+> maybe empty (braces . text) cname <+> args es
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
  text "attached" <+> maybe empty (braces . type') t <+> expr e <+> text "as" <+> text asVar
expr' _ (VarOrCall s)     = text s
expr' _ ResultVar         = text "Result"
expr' _ CurrentVar        = text "Current"
expr' _ LitVoid           = text "Void"
expr' _ (LitChar c)       = quotes (char c)
expr' _ (LitString s)     = anyStringLiteral s
expr' _ (LitInt i)        = int i
expr' _ (LitBool b)       = text (show b)
expr' _ (LitDouble d)     = double d
expr' _ (LitType t)       = braces (type' t)
expr' _ (Cast t e)        = braces (type' t) <+> expr e
expr' _ (Tuple es)        = brackets (hcat $ punctuate comma (map expr es))
expr' _ (Agent e)         = text "agent" <+> expr e
expr' _ (InlineAgent ds resMb ss args)  = 
  let decls = formArgs ds
      res   = maybe empty (\t -> colon <+> type' t) resMb
  in vcat [ text "agent" <+> decls <+> res
          , text "do"
          , nest2 $ vcat (map stmt ss)
          , text "end" <+> condParens (not $ null args)
                                      (commaSep (map expr args))
          ]
expr' _ s                 = error (show s)

condParens True  e = parens e
condParens False e = e

unop Not = "not"
unop Old = "old"

opList = [ (Add, ("+", 6))
         , (Sub, ("-", 6))
         , (Mul, ("*", 7))
         , (Div, ("/", 7))
         , (And, ("and", 4))
         , (AndThen, ("and then", 4))
         , (Or,  ("or", 4))
         , (Xor, ("xor", 4))
         , (OrElse,  ("or else", 4))
         , (Implies, ("implies", 3))
         ]

binop :: BinOp -> (String, Int)
binop (SymbolOp o) = (o, 1)
binop (RelOp r _)  = (relop r, 5)
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