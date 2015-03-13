module ScanLisp where


import ScanLib
import Data.List (intersperse)



data LispToken = LParen
    | RParen
    | LString String
    | LInt Integer
    | LAtom String deriving (Show, Eq)



readStr :: Reader LispToken
readStr str = LString (read str)

readAtom :: Reader LispToken
readAtom a = LAtom a

readLInt :: Reader LispToken
readLInt str = LInt (read str)

readLParen :: Reader LispToken
readLParen = const LParen

readRParen :: Reader LispToken
readRParen = const RParen


isDigit a = elem a "1234567890"
isSpace a = elem a " \n\r\t"

scanLispClauses = [
    Clause 0 (== '"') append 1,
    Clause 1 (/= '"') append 1,
    Clause 1 (== '"') (appendEmit readStr) 0,
    Clause 0 (== '(') (emit readLParen) 0,
    Clause 0 (== ')') (emit readRParen) 0,
    Clause 0 (== '-') append 2,
    Clause 0 (isDigit) append 3,
    Clause 2 (isDigit) append 3,
    Clause 3 (not . isDigit) (emit readLInt) 0,
    Clause 0 (isSpace) ignore 0,
    Clause 0 (const True) append 4,
    Clause 4 (not . isSpace) append 4,
    Clause 4 (isSpace) (emit readAtom) 0]



splitThing :: Eq a => a -> [a] -> [[a]]
splitThing a [] = []
splitThing a s = cons (case break (== a) s of
    (l, s') -> (l, case s' of
        [] -> []
        _:s''   -> splitThing a s''))
    where
        cons (h, t) = h : t

rightParens = splitThing ')'
leftParens = splitThing '('
fixParens text = let
    fixLeftParens str = concat $ intersperse " ( " (leftParens str)
    fixRightParens str = concat $ intersperse " ) " (rightParens str)
    in (fixLeftParens . fixRightParens) (text ++ " ")

scanLispText text = clausesDoStr scanLispClauses (fixParens text)

