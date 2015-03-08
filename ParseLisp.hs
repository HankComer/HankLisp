module ParseLisp where
import Data.List (intersperse)


data LToken = LAtom String |
    LInt Integer |
    LList [LToken] |
    LString String deriving (Show, Eq)

type Parser a = [String] -> Maybe (a, [String])




prependFst :: a -> Maybe ([a], b) -> ([a], b)
prependFst thing (Just (stuff, b)) = (thing:stuff, b)




listParse :: Parser [LToken]
listParse (")":rest) = Just ([], rest)
listParse text = case ultimateParse text of
    Just (thing, rest) -> Just $ prependFst thing (listParse rest)
    Nothing -> Just ([], text)



parseApp :: Parser LToken
parseApp ("(":"if":rest) =
    let
        Just (cond, rest1) = ultimateParse rest
        Just (t, rest2) = ultimateParse rest1
        Just (f, rest3) = ultimateParse rest2
    in Just (LList [LAtom "if", cond, t, f], rest3)
    
parseApp ("(":"define":name:body) = case listParse body of 
    Just (stuff, rest) -> Just (LList (LAtom "define" : LAtom name : stuff), rest)

parseApp ("(":"assign":name:body) = case listParse body of 
    Just (stuff, rest) -> Just (LList (LAtom "assign" : LAtom name : stuff), rest)

parseApp ("(":"lambda":name:rest) =
    let
        Just (args, rest1) = listParse (tail rest)
        Just (body, rest2) = listParse (tail rest1)
    in Just (LList [LAtom "lambda", LList args, LList body], rest2)

parseApp ("(":"defun":name:rest) =
    let
        Just (args, rest1) = listParse (tail rest)
        Just (body, rest2) = listParse (tail rest1)
    in Just (LList [LAtom "assign", LAtom name, LList [LAtom "lambda", LList args, LList body]], rest2)

parseApp ("(":"quote":stuff) = case listParse stuff of
    Just (thing, rest) -> Just (LList (LAtom "quote" : thing), rest)

parseApp ("(":"(":rest) = case parseApp ("(":rest) of
    Just (LList thing, rest1) -> case listParse rest1 of
        Just (thing1, rest2) -> Just (LList (LList thing:thing1), rest2)
    Just (thing, rest1) -> error $ "check parseApp, the one that matches (( " ++ show thing

parseApp ("(":")":rest) = Just (LList [], rest)

parseApp ("(":name:args) = case listParse args of
    Just (thing, rest) -> Just (LList (LAtom name : thing), rest)
    Nothing -> Nothing

parseApp _ = Nothing


parsePrim :: Parser LToken
parsePrim (str:strs) = case (reads str) :: [(Integer, String)] of
    [(a, "")] -> Just (LInt a, strs)
    [] -> case (reads str) :: [(String, String)] of
        [(a, "")] -> Just (LString a, strs)
        [] -> Just (LAtom str, strs)
parsePrim [] = Nothing

ultimateParse text = case parseApp text of
    Just (thing, rest) -> Just (thing, rest)
    Nothing -> parsePrim text



parseString :: String -> [LToken]
parseString str = doThing (tokenize str) where
    doThing :: [String] -> [LToken]
    doThing [] = []
    doThing strs =  case ultimateParse strs of
        Just (stuff, []) -> [stuff]
        Just (stuff, rest) -> stuff : doThing rest
        Nothing -> error $ "parsing " ++ (show str) ++ " failed"


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
tokenize :: String -> [String]
tokenize text = let
    fixLeftParens str = concat $ intersperse " ( " (leftParens str)
    fixRightParens str = concat $ intersperse " ) " (rightParens str)
    in (words . fixLeftParens . fixRightParens) (text ++ " ")