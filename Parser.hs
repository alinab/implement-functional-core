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

pVar :: Parser String
pVar = pSat (\x -> (not . isKWord) x && all Char.isAlpha x)

pNum :: Parser Int
pNum = pSat (all Char.isDigit)
        `pApply`
           (\x -> Maybe.fromMaybe 0 (Read.readMaybe x :: Maybe Int))

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

pExpr :: Parser CoreExpr
pExpr toks
  | [(n, toks')] <- pNum toks               = [(Lang.ENum n, toks')]
  | [(v, toks')] <- pVar toks               = [(Lang.EVar (show v), toks')]
