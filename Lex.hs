module Lex where

import Language as Lang
import Data.List as List
import Data.Char as Char

type Token = (Int, [Char])

clex :: String -> Int -> [Token]
clex [] lnum                                  = [(lnum, [])]
clex (c : cs) lnum | Char.isSpace c           = clex cs lnum
clex (c : cs) lnum | isArithOp c              = (lnum, [c]) : clex cs lnum
clex (c : cs) lnum | isRelOpOneC c            = (lnum, [c]) : clex cs lnum
clex (c : cs) lnum | Char.isDigit c           = (lnum , num_token) : clex rest_cs lnum
                                                 where
                                                 num_token = c : List.takeWhile isDigit cs
                                                 rest_cs   = List.dropWhile isDigit cs

clex (c : cs) lnum | Char.isAlpha c           = (lnum, var_token) : clex rest_cs lnum
                                                 where
                                                 var_token = c : List.takeWhile isIdChar cs
                                                 rest_cs   = List.dropWhile isIdChar cs
clex (c : d : es) lnum | isRelOpTwoC (c:[d])  = (lnum, c:[d]) : clex es lnum
clex (c : d : es) lnum | isBoolOp (c :[d])    = (lnum, c:[d]) : clex es lnum
clex (c : d : es) lnum | c == '-' && d == '-' = clex rest_es lnum
                                                 where
                                                 (ig_comment_line, (i : j : rest_es))
                                                   = List.span (\x -> isIdChar x && x /= '-') es

isIdChar :: Char -> Bool
isIdChar c = Char.isAlpha c || Char.isDigit c || c == '_'

isArithOp :: Char -> Bool
isArithOp c = c `elem` ['+', '-', '*', '/']

isBoolOp :: String -> Bool
isBoolOp c = c `elem` ["&&", "||"]

isRelOpOneC :: Char -> Bool
isRelOpOneC c = c `elem` ['<', '>']

isRelOpTwoC :: String -> Bool
isRelOpTwoC c = c `elem` ["==", "~=", ">=", "<=", "->"]

--syntax :: [Token] -> Lang.CoreProgram

--parse :: String -> Lang.CoreProgram
--parse = syntax . clex
