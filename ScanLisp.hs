module ScanLisp where


import ScanLib
import Data.List (intersperse)



data LispToken = LParen
    | RParen
    | LStringT String
    | LIntT Integer
    | LAtomT String deriving (Show, Eq)



unread :: LispToken -> String
unread LParen = "("
unread RParen = ")"
unread (LStringT foo) = show foo
unread (LIntT bar) = show bar
unread (LAtomT baz) = baz

readStr :: Reader LispToken
readStr str = LStringT (read str)

readAtom :: Reader LispToken
readAtom a = LAtomT a

readLIntT :: Reader LispToken
readLIntT str = LIntT (read str)

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
    Clause 3 (not . isDigit) (emitPush readLIntT) 0,
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

tokenize text = case scanLispText (removeComments text) of
    (_, (_, _, stuff)) -> stuff

removeComments text = unlines $ map (takeWhile (/= ';')) (lines text)


untoken :: [LispToken] -> String
untoken tokens = unwords $ map unread tokens

