module Language where

import           Data.List as List

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
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
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

iNil :: Iseq
iNil                          = INil

iAppend :: Iseq -> Iseq -> Iseq
iAppend seq1 seq2             = IAppend seq1 seq2

iStr :: String -> Iseq
iStr str                      = IStr str

iNum :: Int -> Iseq
iNum n                        = IStr (show n)

iNewline :: Iseq
iNewline                      = IStr "\n"

iIndent :: Iseq -> Iseq
iIndent iseq                  = IIndent iseq

iConcat :: [Iseq] -> Iseq
iConcat []                    = INil
iConcat [seq1]                = iAppend seq1 INil
iConcat (seq1 : seq2 : seqLs) = iAppend seq1 (iConcat (seq2 : seqLs))

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ []              = INil
iInterleave _ [seq1]          = iAppend seq1 INil
iInterleave sep (seq1 : seqLs)
                              = iAppend sep (iAppend seq1 (iInterleave sep seqLs))

-- convert core expressions to use Iseq types --
pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v)                = iStr v
pprExpr (ENum n)                = iNum n
pprExpr (EApp (EApp (EVar "+") e1) e2)
                                = iConcat [ pprAExpr e1, iStr " + ", pprAExpr e2 ]
pprExpr (EApp (EApp (EVar "-") e1) e2)
                                = iConcat [ pprAExpr e1, iStr " - ", pprAExpr e2 ]
pprExpr (EApp (EApp (EVar "*") e1) e2)
                                = iConcat [ pprAExpr e1, iStr " * ", pprAExpr e2 ]
pprExpr (EApp (EApp (EVar "/") e1) e2)
                                = iConcat [ pprAExpr e1, iStr " / ", pprAExpr e2 ]
pprExpr (EApp e1 e2)            = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
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
mkMultiAp n e1 e2 = List.foldr EApp e1 (List.take n e2s)
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
    for it is linear to the input -}

flatten :: Int                 -- tracks the current column with 0 as the first
        -> [(Iseq, Int)]       -- list of iseq's to flatten
        -> String              -- output
flatten _  []                                    = ""
flatten col ((INil, _) : seqs)                   = flatten col seqs
flatten col ((IStr s, _) : seqs)                 = s ++ flatten col seqs
flatten col ((INum i, _) : seqs)                 = show i ++ flatten col seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) :
                                                                (seq2, indent) :
                                                                seqs)
flatten _   ((INewline, indent) : seqs)          = '\n' : space indent ++ flatten indent seqs
flatten col ((IIndent seq1, _) : seqs)           = flatten col ((seq1, col) : seqs)

space :: Int -> String
space i = List.replicate i ' '

iDisplay :: Iseq -> String
iDisplay iseq = flatten 0 [(iseq, 0)]

pprExprWithDisplay :: CoreExpr -> String
pprExprWithDisplay (EVar v)       = iDisplay (iStr v)
pprExprWithDisplay (ENum n)       = iDisplay (iNum n)
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

-- ensure that all numbers are aligned the same --
iFWNum :: Int ->  Int -> Iseq
iFWNum width n = iStr (space (width - List.length digits) ++ digits)
                    where
                      digits = show n

-- number each item in a list with a newline char. after each item --
iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (List.zipWith (curry lay_item) [1 .. ] seqs)
                where
                   lay_item (n, s)
                    = iConcat [ iFWNum 4 n, iStr ") ", iIndent s, iNewline ]
