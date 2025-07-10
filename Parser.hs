{-# LANGUAGE ExplicitNamespaces #-}

module Parser where

import Lex
import Data.Maybe as Maybe
import Text.Read as Read
import Data.Char as Char

import qualified Language as Lang (type CoreProgram, type CoreScDefn,
                                   type CoreExpr, type Name, type CoreAlt,
                                   Expr(EVar), Expr(ENum),
                                   Expr(ELet), Expr (ECase),
                                   rhssOf)

type Parser a = [Token] -> [(a, [Token])]

{--- Parsers ---}
pLit :: String -> Parser String
pLit s (tok : toks) | matchToken s tok  = [(s, toks)]
                    | otherwise         = []
pLit _ []                               = []

matchToken :: String -> Token -> Bool
matchToken s token = let (_ , tokStr) = token
                     in s == tokStr

pVar' :: Parser String
pVar' [] = []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks


pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks,
                                                      (v2, toks2) <- p2 toks1 ]

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")


pGreeting :: Parser (String, String)
pGreeting = pThen mkPair pHelloOrGoodbye pVar'
             where
               mkPair hg name = (hg, name)

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine3 p1 p2 p3 toks = [ (combine3 v1 v2 v3, toks3)
                                               | (v1, toks1) <- p1 toks,
                                                 (v2, toks2) <- p2 toks1,
                                                 (v3, toks3) <- p3 toks2 ]

pGreeting' :: Parser (String, String)
pGreeting' = pThen3 mk_greeting pHelloOrGoodbye (pLit "James") (pLit "!")
             where
               mk_greeting hg name _ = (hg, name)


pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b
                                  -> Parser c -> Parser d -> Parser e
pThen4 combine3 p1 p2 p3 p4 toks = [ (combine3 v1 v2 v3 v4, toks4)
                                                    | (v1, toks1) <- p1 toks,
                                                      (v2, toks2) <- p2 toks1,
                                                      (v3, toks3) <- p3 toks2,
                                                      (v4, toks4) <- p4 toks3 ]

pEmpty :: a -> Parser a
pEmpty s toks = [(s, toks)]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pOneOrMore :: Parser a  -> Parser [a]
pOneOrMore p toks  = oneOrMore p toks []
    where
        oneOrMore p' toks' tokls
             | null toks'            = []
             | [(v, t')] <- p' toks' = oneOrMore p' t' (v : tokls)
             |         otherwise     = [(tokls, toks')]

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting'

pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting') `pApply` length

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f tok
    | [(m, tok')] <- p tok = [(f m, tok')]
    | otherwise            = []

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 toks
  | tk@[(v, tok')] <- p1 toks = if null tk
                                then pOneOrMore p1 toks
                                else ([v], rest tok') : pOneOrMoreWithSep
                                                            p1 p2 (rest tok')
                                where
                                  rest tok' = concat $ Lang.rhssOf (p2 tok')

pOneOrMoreWithSep  _ _ (_:_)  = []
pOneOrMoreWithSep  _ _ []     = []

pSat :: (String -> Bool) -> Parser String
pSat sTest ((_, s) : toks) | sTest s   = [(s, toks)]
                           | otherwise = []
pSat _          []                     = []

pLit' :: String -> Parser String
pLit' s = pSat (== s)

pVar'' :: String -> Parser String
pVar'' v = pSat (== v)

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

isKWord :: String -> Bool
isKWord w = w `elem` keywords

{-----------------------------------------------------------------------------}
{-- Parser for Core --}

syntax :: [Token] -> Lang.CoreProgram
syntax = take_first_parse . pProgram
            where
            take_first_parse ((prog, []) : _)      = prog
            take_first_parse ((prog, _ ) : others) = prog ++ take_first_parse others
            take_first_parse  _                    = error "Parse error: Wrong syntax"

pProgram :: Parser Lang.CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser Lang.CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

mkSc :: Lang.Name -> [Lang.Name] -> Lang.Name
                  -> Lang.CoreExpr -> Lang.CoreScDefn
mkSc v1 v2 _ v4 = (v1, v2, v4)

pVar :: Parser String
pVar = pSat (\x -> (not . isKWord) x && all Char.isAlpha x)

pNum :: Parser Int
pNum = pSat (all Char.isDigit)
        `pApply`
           (\x -> Maybe.fromMaybe 0 (Read.readMaybe x :: Maybe Int))

pLet :: Parser ((Lang.Name, Lang.CoreExpr), Lang.CoreExpr)
pLet = pThen4 mkPLet (pLit "let") pScLet (pLit "in") pExpr

mkPLet :: Lang.Name -> (Lang.Name, Lang.CoreExpr)
                    -> Lang.Name
                    -> Lang.CoreExpr
                    -> ((Lang.Name, Lang.CoreExpr), Lang.CoreExpr)
mkPLet _ v2 _ v4 = (v2, v4)

pScLet :: Parser (Lang.Name, Lang.CoreExpr)
pScLet = pThen3 mkLetDefn pVar (pLit "=") pExpr

mkLetDefn :: Lang.Name -> Lang.Name -> Lang.CoreExpr
                        -> (Lang.Name, Lang.CoreExpr)
mkLetDefn v1 _ v3 = (v1, v3)

pCase :: Parser (Lang.CoreExpr, [Lang.CoreAlt])
pCase = pThen4 mkpCase (pLit "case")
                       (pThen3 pCaseExpr (pAlt (pLit "(") (pLit ""))
                                pExpr (pAlt (pLit ")") (pLit "")))
                       (pLit "of") pCaseAlt

pCaseExpr :: String -> Lang.CoreExpr -> String -> Lang.CoreExpr
pCaseExpr _ v2 _ = v2

mkpCase :: String -> Lang.CoreExpr -> String
                  -> [Lang.CoreAlt] -> (Lang.CoreExpr, [Lang.CoreAlt])
mkpCase _ v2 _ v4 = (v2, v4)

pCaseAlt :: Parser [Lang.CoreAlt]
pCaseAlt = pOneOrMore (pThen3 pCaseAltExpr
                      (pThen4 pCaseNum (pLit "<") pNum (pLit ">") (pLit "->"))
                              pExpr (pAlt (pLit ";") (pLit "")))

pCaseNum :: String -> Int -> String -> String -> Int
pCaseNum _ v2 _ _ = v2

pCaseAltExpr :: Int -> Lang.CoreExpr -> String -> Lang.CoreAlt
pCaseAltExpr v1 v2 _ = (v1, [], v2)


pExpr :: Parser Lang.CoreExpr
pExpr toks
  | [(n, toks')] <- pNum toks               = [(Lang.ENum n, toks')]
  | [(v, toks')] <- pVar toks               = [(Lang.EVar (show v), toks')]
  | [((defns, expr), toks')] <- pLet toks   = [(Lang.ELet False [defns] expr, toks')]
  | [((expr, alters), toks')] <- pCase toks = [(Lang.ECase expr alters, toks')]
