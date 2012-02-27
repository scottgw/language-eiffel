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
emptyLine = text ""

ups = map toUpper
toDoc c =
  let defer = if deferredClass c then text "deferred" else empty
  in vsep [ notes (classNote c)
          , defer <+> text "class"
          , nest2 (text (ups $ className c)) <+> genericsDoc (generics c) <+> procGenDoc (procGeneric c)
          , emptyLine
          , inheritClauses (inherit c)
          , vsep (map createClause (creates c))
          , convertClause (converts c)
          , vsep (map featureClause (featureClauses c))
          , invars (invnts c)
          , text "end"
          ]

inheritClauses cs =
  text "inherit" $?$ nest2 (vsep (map inheritClause cs))

inheritClause (InheritClause cls renames exports undefs redefs selects) = 
  let renameDoc (Rename orig new alias) =
        text orig <+> text "as" <+> text new <+> 
          maybe empty (\a -> text "alias" <+> doubleQuotes (text a)) alias
      exportListDoc (ExportFeatureNames l) = vCommaSep (map text l)
      exportListDoc ExportAll = text "all"
      exportDoc (Export to what) =
        braces (commaSep (map text to)) $+$ nest2 (exportListDoc what)
  in type' cls $+$ nest2 (vsep
          [ text "rename" $?$ nest2 (vCommaSep (map renameDoc renames))
          , text "export" $?$ nest2 (vsep (map exportDoc exports))
          , text "undefine" $?$ nest2 (vCommaSep (map text undefs))
          , text "redefine" $?$ nest2 (vCommaSep (map text redefs))
          , text "select" $?$ nest2 (vCommaSep (map text selects))
          , if null renames && null exports && null undefs && null redefs && null selects
            then empty else text "end" $+$ emptyLine
          ])
      
createClause (CreateClause exports names) = 
  let exps = if null exports 
             then empty 
             else  braces (commaSep (map text exports))
  in (text "create" <+> exps) $+$ nest2 (commaSep (map text names)) $+$ emptyLine
  
convertClause []    = empty
convertClause convs =
  let go (ConvertFrom fname t) = text fname <+> parens (braces (type' t))
      go (ConvertTo fname t) = text fname <> colon <+> braces (type' t)
  in text "convert" $+$ nest2 (vCommaSep (map go convs))

featureClause (FeatureClause exports featrs attrs consts) = 
  let exps = if null exports 
             then empty 
             else  braces (commaSep (map text exports))
  in vsep [ text "feature" <+> exps
          , emptyLine
          , nest2 $ vsep $ map (($+$ emptyLine) . featureDoc) featrs
          , nest2 $ vsep $ map (($+$ emptyLine) . attrDoc) attrs
          , nest2 $ vsep $ map (($+$ emptyLine) . constDoc) consts
          ]


vsep = foldr ($+$) empty
commaSep = hcat . punctuate comma
vCommaSep = vsep . punctuate comma
angles d = langle <> d <> rangle
langle = char '<'
rangle = char '>'
squareQuotes t = text "\"[" <> t <> text "]\""
                      
anyStringLiteral s = case s of
  '\n':_      -> squareQuotes $ text s
  '\r':'\n':_ -> squareQuotes $ text s
  _           -> doubleQuotes $ text s


procDoc (Proc s) = text s

genericsDoc [] = empty
genericsDoc gs = brackets (commaSep (map go gs))
  where go (Generic name constr) = text name <+> constraints constr
        constraints []           = empty
        constraints [t]          = text "->" <+> type' t
        constraints ts           = text "->" <+> braces (commaSep (map type' ts))

notes [] = empty
notes ns = vsep [ text "note"
                , nest2 (vsep $ map note ns)
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

attrDoc :: Attribute Expr -> Doc
attrDoc (Attribute d assn ns reqs ens) = 
  decl d <+> vsep [ assignText assn
                  , notes ns
                  , reqText reqs
                  , attrKeyword
                  , ensText ens
                  , endKeyword
                  ]
  where assignText Nothing  = empty
        assignText (Just a) = text "assign" <+> text a
        ensText []  = empty 
        ensText es  = text "ensure" $?$ clausesDoc es
        reqText []  = empty
        reqText rs  = text "require" $?$ clausesDoc rs
        hasBody     = not (null ens && null reqs && null ns)
        attrKeyword | hasBody   = text "attribute"
                    | otherwise = empty
        endKeyword  | hasBody   = text "end"
                    | otherwise = empty

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
      in header <+> assign $+$ 
          (nest2 $ vsep 
           [ notes (featureNote f)
           , text "require" $?$ clausesDoc (featureReq f) 
           , text "require-order" $?$ nest2 (procExprs f)
           , text "lock" $?$ nest2 (locks (featureEnsLk f))
           , featureBodyDoc $ featureImpl f
           , text "ensure" $?$ clausesDoc (featureEns f)
           , text "end"
           ]
          )

featureBodyDoc FeatureDefer = text "deferred"
featureBodyDoc ft = vsep [ locals ft
                         , text "do"
                         , nest2 $ stmt $ featureBody ft
                         ]

locals ft = text "local" $?$ nest2 (vsep $ map decl (featureLocal ft))

procExprs = vCommaSep . map procExprD . featureReqLk

($?$) :: Doc -> Doc -> Doc
($?$) l e 
    | isEmpty e = empty
    | otherwise = l $+$ e

clausesDoc :: [Clause Expr] -> Doc
clausesDoc cs = nest2 (vsep $ map clause cs)

clause :: Clause Expr -> Doc
clause (Clause nameMb e) = maybe empty (\n -> text n <> colon) nameMb <+> expr e

nest2 = nest 2

stmt = stmt' . contents

stmt' (Assign l e) = expr l <+> text ":=" <+> expr e
stmt' (AssignAttempt l e) = expr l <+> text "?=" <+> expr e
stmt' (CallStmt e) = expr e
stmt' (If cond body elseParts elseMb) = 
  let elsePart = case elseMb of
        Just elsee -> vsep [text "else", nest2 (stmt elsee)]
        Nothing -> empty
      elseifPart (ElseIfPart c s) =
        vsep [ text "elseif" <+> expr c <+> text "then"
             , nest2 (stmt s)
             ]
      elseifParts es = vsep (map elseifPart es)
  in vsep [ text "if" <+> expr cond <+> text "then"
          , nest2 (stmt body)
          , elseifParts elseParts
          , elsePart
          , text "end"
          ]
stmt' (Inspect e whens elseMb) =
  let elsePart = case elseMb of
        Nothing -> empty
        Just s -> text "else" $+$ nest2 (stmt s)
      whenParts (e,s) = (text "when" <+> expr e <+> text "then") $+$ 
                        nest2 (stmt s)
  in vsep [ text "inspect" <+> expr e
          , vsep (map whenParts whens)
          , elsePart
          , text "end"
          ]
stmt' (BuiltIn)  = text "builtin"
stmt' (Create t tar n es) = text "create" <+> maybe empty (braces . type') t <+> expr' 0 (QualCall tar n es)
stmt' (DefCreate t v) = text "create" <+> maybe empty (braces . type') t <+> expr v
stmt' (Block ss) = vsep (map stmt ss)
stmt' (Check cs) = vsep [ text "check"
                        , nest2 (vsep (map clause cs))
                        , text "end"
                        ]
stmt' (Loop from invs until loop) = 
  vsep [ text "from"
       , nest2 (stmt from)
       , text "invariant" $?$ clausesDoc invs
       , text "until"
       , nest2 (expr until)
       , text "loop"
       , nest2 (stmt loop)
       , text "end"
       ]
stmt' (Debug str body) = 
  vsep [ text "debug" <+> if null str then empty else (parens . anyStringLiteral) str
       , nest2 (stmt body)
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
                 _ -> exprPrec 13 t <> char '.'
expr' _ (PrecursorCall cname es) = text "Precursor" <+> maybe empty (braces . text) cname <+> args es
-- expr' _ (StaticCall t n) = braces (type' t) <> char '.' <> text n
expr' i (UnOpExpr uop e) = condParens (i > 12) $ text (unop uop) <+> exprPrec 12 e
expr' i (BinOpExpr (SymbolOp op) e1 e2)
  | op == "[]" = exprPrec i e1 <+> brackets (expr e2)
  | otherwise =  condParens (i > 11) 
                 (exprPrec 11 e1 <+> text op <+> exprPrec 12 e2)
expr' i (BinOpExpr bop e1 e2) = 
  condParens (i > p) 
             (exprPrec lp e1 <+> text op <+> exprPrec rp e2)
  where (op, p) = binop bop
        lp = p
        rp = p + 1
expr' _ (Attached t e asVar) = 
  text "attached" <+> maybe empty (braces . type') t <+> expr e <+> maybe empty (\s -> text "as" <+> text s) asVar
expr' _ (CreateExpr t n es) = text "create" <+> braces (type' t) <> char '.' <> text n <+> args es
expr' _ (VarOrCall s)     = text s
expr' _ ResultVar         = text "Result"
expr' _ CurrentVar        = text "Current"
expr' _ LitVoid           = text "Void"
expr' _ (LitChar c)       = quotes (char c)
expr' _ (LitString s)     = anyStringLiteral s
expr' _ (LitInt i)        = int i
expr' _ (LitBool b)       = text (show b)
expr' _ (LitDouble d)     = double d
expr' _ (LitStaticClass t) = braces (type' t)
expr' _ (LitType t)       = parens $ braces (type' t)
expr' _ (Cast t e)        = braces (type' t) <+> expr e
expr' _ (Tuple es)        = brackets (hcat $ punctuate comma (map expr es))
expr' _ (Agent e)         = text "agent" <+> expr e
expr' _ (InlineAgent ds resMb ss args)  = 
  let decls = formArgs ds
      res   = maybe empty (\t -> colon <+> type' t) resMb
  in vsep [ text "agent" <+> decls <+> res
          , text "do"
          , nest2 $ vsep (map stmt ss)
          , text "end" <+> condParens (not $ null args)
                                      (commaSep (map expr args))
          ]
expr' _ s                 = error (show s)

condParens True  e = parens e
condParens False e = e

unop Neg = "-"
unop Not = "not"
unop Old = "old"

opList = [ (Pow, ("^", 10))
         , (Mul, ("*", 9))
         , (Div, ("/", 9))
         , (Quot, ("//", 9))
         , (Rem, ("\\\\", 9))
         , (Add, ("+", 8))
         , (Sub, ("-", 8))
         , (And, ("and", 5))
         , (AndThen, ("and then", 5))
         , (Or,  ("or", 4))
         , (Xor, ("xor", 4))
         , (OrElse,  ("or else", 4))
         , (Implies, ("implies", 3))
         ]

binop :: BinOp -> (String, Int)
binop (SymbolOp o) = (o, 11)
binop (RelOp r _)  = (relop r, 6)
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
relop TildeNeq = "/~"

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