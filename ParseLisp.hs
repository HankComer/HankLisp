module ParseLisp where
import ScanLisp
import Data.List (intersperse)

data LType = LList [LType]
    | LInt Integer
    | LString String
    | LAtom String
    | NilT deriving (Show, Eq)


convert (LIntT a) = LInt a
convert (LStringT a) = LString a
convert (LAtomT a) = LAtom a
convert foo = error (show foo ++ " isn't covered by convert")

type Parser a = [LispToken] -> Maybe (a, [LispToken])




prependFst :: a -> Maybe ([a], b) -> ([a], b)
prependFst thing (Just (stuff, b)) = (thing:stuff, b)




listParse :: Parser [LType]
listParse (RParen:rest) = Just ([], rest)
listParse text = case ultimateParse text of
    Just (thing, rest) -> Just $ prependFst thing (listParse rest)
    Nothing -> Just ([], text)



parseApp :: Parser LType
parseApp (LParen:(LAtomT "if"):rest) =
    let
        Just (cond, rest1) = ultimateParse rest
        Just (t, rest2) = ultimateParse rest1
        Just (f, rest3) = ultimateParse rest2
    in Just (LList [LAtom "if", cond, t, f], rest3)
    
parseApp (LParen:(LAtomT "define"):name:body) = case listParse body of 
    Just (stuff, rest) -> Just (LList (LAtom "define" : convert name : stuff), rest)

parseApp (LParen:(LAtomT "assign"):name:body) = case listParse body of 
    Just (stuff, rest) -> Just (LList (LAtom "assign" : convert name : stuff), rest)

parseApp (LParen:(LAtomT "lambda"):rest) =
    let
        Just (args, rest1) = listParse (tail rest)
        Just (body, rest2) = listParse (tail rest1)
    in Just (LList [LAtom "lambda", LList args, LList body], rest2)

parseApp (LParen:(LAtomT "defun"):name:rest) =
    let
        Just (args, rest1) = listParse (tail rest)
        Just (body, rest2) = listParse (tail rest1)
    in case rest2 of
        RParen:rest3 -> Just (LList [LAtom "assign", convert name, LList [LAtom "lambda", LList args, LList body]], rest3)

parseApp (LParen:(LAtomT "quote"):stuff) = case listParse stuff of
    Just (thing, rest) -> Just (LList (LAtom "quote" : thing), rest)

parseApp (LParen:LParen:rest) = case parseApp (LParen:rest) of
    Just (LList thing, rest1) -> case listParse rest1 of
        Just (thing1, rest2) -> Just (LList (LList thing:thing1), rest2)
    Just (thing, rest1) -> error $ "check parseApp, the one that matches (( " ++ show thing

parseApp (LParen:RParen:rest) = Just (NilT, rest)

parseApp (LParen:(LAtomT name):args) = case listParse args of
    Just (thing, rest) -> Just (LList (LAtom name : thing), rest)
    Nothing -> Nothing

parseApp _ = Nothing


parsePrim :: Parser LType
parsePrim (foo:rest) = Just (convert foo, rest)
parsePrim [] = Nothing

ultimateParse text = case parseApp text of
    Just (thing, rest) -> Just (thing, rest)
    Nothing -> parsePrim text



parseString :: String -> [LType]
parseString str = doThing (tokenize str) where
    doThing :: [LispToken] -> [LType]
    doThing [] = []
    doThing strs =  case ultimateParse strs of
        Just (stuff, []) -> [stuff]
        Just (stuff, rest) -> stuff : doThing rest
        Nothing -> error $ "parsing " ++ (show str) ++ " failed"




