module Parser where

import Lex
import Data.Foldable as DFold

-- The type Token is a list of (Int, [Char]) --
type Parser a = [Token] -> [(a, [Token])]

{--- Parsers ---}
pLit :: String -> Parser String
pLit s (tok : toks) | matchToken s tok  = [(s, toks)]
                    | otherwise         = []
pLit _ []                               = []

matchToken :: String -> Token -> Bool
matchToken s token = let (_ , tokStr) = token
                     in s == tokStr

pVar :: Parser String
pVar [] = []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks


pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks,
                                                      (v2, toks2) <- p2 toks1 ]

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")


pGreeting :: Parser (String, String)
pGreeting = pThen mk_pair pHelloOrGoodbye pVar
             where
               mk_pair hg name = (hg, name)

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
pOneOrMore p toks
  | tk@[(v, tok')] <- p toks = if null tk
                               then pOneOrMore p tok'
                               else ([v], tok') : pOneOrMore p tok'
pOneOrMore _ (_:_)           = []
pOneOrMore _ []              = []

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting'

pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting') `pApply` length

pApply :: Num b => Parser a -> (a -> b) -> Parser b
pApply p f toks
  | tk <- p toks = let y = map (f . fst) tk
                   in [(DFold.sum y, [])]
