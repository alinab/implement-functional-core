module CoreLanguage where

import           Data.List as List
import qualified Data.Text as T


data Expr e = EVar Name               -- Variables
            | ENum Int                -- Numbers
            | EConstr Int Int         -- Constructor tag arity
            | EApp (Expr e) (Expr e)  -- Applications
            | ELet                    -- Let(rec) expressions
                IsRec                 --    True when recursive
                [(e, Expr e)]         --    Defintions
                (Expr e)              --    Body of the let(rec) expression
            | ECase                   -- Case expressions
                (Expr e)              --    Expression for case to compute
                [Alter e]             --    Alternatives to match
            | ELam [e] (Expr e)       -- Lambda abstractions
             deriving (Show)


type CoreExpr = Expr Name
type Name     = String
type IsRec    = Bool

recursive :: IsRec
recursive = True

nonRecursive :: IsRec
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name , rhs) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-- Expressions with no internal structure are atomic --
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

-- a supercombinator defintion consists of a name, its arguments
-- and the body of the definition
type ScDefn a    = (Name, [a], Expr a)
type CoreScDefn  = ScDefn Name

-- a program is a list of supercombinator definitions
type Program p   = [ScDefn p]
type CoreProgram = Program Name

-- a minimal prelude for core language including the I, K , K1
-- S combinators and functions for composition --
preludeDefs :: CoreProgram
preludeDefs = [("I", ["x"], EVar "x"),
               ("K", ["x", "y"], EVar "x"),
               ("K1", ["x", "y"], EVar "y"),
               ("S", ["f", "g", "h"], EApp (EApp (EVar "f") (EVar "x"))
                                           (EApp (EVar "g") (EVar "x"))),
               ("compose", ["f", "g", "x"], EApp (EVar "f")
                                                 (EApp (EVar "g") (EVar "x"))),
               ("twice", ["f"], EApp (EApp (EVar "compose") (EVar "f")) (EVar "f"))]


-------------------------------------------------------------------------------
-- printing with abstract type Iseq --
data Iseq = INil
          | IStr String
          | INum Int
          | IAppend Iseq Iseq
          | INewline
          | IIndent Iseq
          | IDisplay Iseq

iNil                          = INil
iAppend seq1 seq2             = IAppend seq1 seq2
iStr str                      = IStr str
iNum n                        = INum n
iConcat [INil]                = INil
iConcat [seq1]                = iAppend seq1 INil
iConcat (seq1 : seq2 : seqLs) = iAppend seq1 (iConcat (seq2 : seqLs))
iInterleave sep [INil]        = INil
iInterleave sep ([seq1])      = iAppend seq1 INil
iInterleave sep (seq1 : seqLs)
                              = iAppend sep (iAppend seq1 (iInterleave sep seqLs))
iNewline                      = IStr "\n"
iIndent seq                   = IIndent seq

-- convert core expressions to use Iseq types --
pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v)                = iAppend (iStr " = ") (iStr v)
pprExpr (EApp e1 e2)            = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprExpr e2)
pprExpr (ELet isrec defns expr) = iConcat [iStr keyword , iNewline,
                                           iStr " ", iIndent (pprDefns defns), iNewline,
                                           iStr "in ", pprExpr expr]
                                    where
                                      keyword | not isrec = "let"
                                              | isrec = "letrec"
pprExpr (ECase exp1 alterLs)  = iConcat [ iStr "case ", pprExpr exp1, iStr "of",
                                          iNewline,
                                          iIndent (iConcat (List.concatMap showAlters alterLs)) ]
                                 where
                                   showAlters (i, boundVars, expr) =
                                       [iStr "<" , iNum i, iStr ">",
                                                iInterleave (IStr ", ") (map iStr boundVars),
                                                iStr "=>", pprExpr expr, iNewline]

pprExpr (ELam varLs expr) = iConcat [ iStr "\\", iInterleave (IStr ", ") (map iStr varLs),
                                      iNewline, iIndent (pprExpr expr)]



pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
                    where
                     sep = iConcat [ iStr ";" , iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]


pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
pprAExpr e | otherwise      = iConcat [IStr "(", pprExpr e, IStr ")"]

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = List.foldl EApp e1 (List.take n e2s)
                    where
                      e2s = e2 : e2s

pprProgram :: CoreProgram -> Iseq
pprProgram scDefLs = iConcat (List.concatMap showScDef scDefLs)
                        where
                          showScDef (name, nameLs, nameExpr) =
                             [iStr (name ++ space 2),
                             iInterleave (IStr " ") (map (\x -> iStr (x ++ space 1)) nameLs),
                             pprExpr nameExpr, iNewline]

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

-------------------------------------------------------------------------------
{- Flattening Iseqs into displayable strings while ensuring that the time taken
    for it is linear to the input iseq -}

flatten :: Int -> [(Iseq, Int)] -> String
flatten col []                                   = ""
flatten col ((INil, indent) : seqs)              = flatten col seqs
flatten col ((IStr s, indent) : seqs)            = s ++ flatten col seqs
flatten col ((INum i, indent) : seqs)            = show i ++ flatten col seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) :
                                                                (seq2, indent) :
                                                                seqs)
flatten col ((INewline, indent) : seqs)          = '\n' : (space indent) ++ flatten indent seqs
flatten col ((IIndent seq, indent) : seqs)       = flatten col ((seq, col) : seqs)

space :: Int -> String
space i = List.replicate i ' '

iDisplay seq = flatten 0 [(seq, 0)]

pprExprWithDisplay :: CoreExpr -> String
pprExprWithDisplay (EVar v)       = iDisplay (iStr v)
pprExprWithDisplay (EApp e1 e2)   = iDisplay $ (pprExpr e1) `iAppend` (iStr " ")
                                                                `iAppend` (pprExpr e2)
pprExprWithDisplay (ELet isrec defns expr) = iDisplay $ iConcat [iStr keyword,
                                                            iNewline, iStr " ",
                                                            iIndent (pprDefns defns),
                                                            iNewline, iStr "in ", pprExpr expr]
                                                 where
                                                   keyword | not isrec = "let"
                                                           | isrec = "letrec"
pprExprWithDisplay (ECase exp1 alterLs)  = iDisplay $ iConcat [ iStr "case ",
                                                pprExpr exp1, iStr "of", iNewline,
                                                iIndent (iConcat (List.concatMap showAlters alterLs)) ]
                                              where
                                                showAlters (i, boundVars, expr) =
                                                   [iStr "<" , iNum i, iStr ">",
                                                    iInterleave (IStr ", ") (map iStr boundVars),
                                                    iStr "=>", pprExpr expr, iNewline]

pprExprWithDisplay (ELam varLs expr) = iDisplay $ iConcat [ iStr "\\",
                                        iInterleave (IStr ", ") (map iStr varLs),
                                                iNewline, iIndent (pprExpr expr)]
