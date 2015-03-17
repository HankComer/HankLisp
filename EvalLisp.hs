module EvalLisp where

import ParseLisp
import Control.Applicative
import Control.Monad (join)

type Environment = [(String, LValue)]

type LFunctionT = Environment -> IO LValue -> IO LValue

infixr 7 :.
data LValue = Atom String |
    Number Integer |
    LValue :. LValue |
    LFunction LFunctionT String |
    Str String |
    Nil |
    Change Environment

cons :: LValue -> LValue -> LValue
cons a b = a :. b

lispList :: [LValue] -> LValue
lispList ([]) = Nil
lispList (a:[]) = a :. Nil
lispList (a:as) = a :. lispList as

haskList :: LValue -> [LValue]
haskList Nil = []
haskList (a:.Nil) = a:[]
haskList (a:.b) = a : haskList b
haskList (a) = [a]

instance Show LValue where
    show (Atom str) = str
    show (Number i) = show i
    show Nil = "()"
    show (a:.b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (LFunction func meta) = "function " ++ meta
    show (Str str) = show str

instance Eq LValue where
    (Atom a) == (Atom b) = a == b
    (Number i) == (Number j) = i == j
    (a:.b) == (c:.d) = (a == c) 
    (Str a) == (Str b) = a == b
    _ == _ = False
    
instance Ord LValue where
    (a:._) <= (b:._) = a <= b
    (a:._) <= b = a <= b
    Nil <= Nil = True
    Nil <= a = True
    a <= Nil = False
    a <= (b:._) = a <= b
    (LFunction _ _) <= a = True
    a <= (LFunction _ _) = False
    a <= b = (extract a) <= (extract b)

extract :: LValue -> String
extract (Atom a) = a
extract (Str a) = a
extract (Number n) = show n

tok2Val :: LType -> LValue
tok2Val (LAtom a) = Atom a
tok2Val (LInt i) = Number i
tok2Val (LList ts) = lispList (map tok2Val ts)
tok2Val (LString s) = Str s
tok2Val NilT = Nil

toks2Vals = map tok2Val

nil = Nil

unsafeLookup :: String -> Environment -> LValue
unsafeLookup a b = case lookup a b of
    Just r -> r
    Nothing -> Atom a

updateEnvironment :: Environment -> (String, LValue) -> Environment
updateEnvironment = flip (:)


deref :: Environment -> LValue -> LValue
deref env (Atom name) = unsafeLookup name env
deref env (Number i) = Number i

createFunction :: Environment -> LValue -> LValue -> LValue
createFunction env (a:.b) body = LFunction lispFunction metaData where
    args = (a:.b)
    lispFunction :: LFunctionT
    lispFunction env' args' = args' >>= (\args''-> (fmap snd) $ lispEval(zip (lmapToList extract args) (haskList args'') ++ env', body))
    metaData :: String
    metaData = show args ++ " -> " ++ show body



inscope env name = case lookup name env of
    Just _ -> True
    Nothing -> False



lmapToList f (a:.Nil) = (f a : [])
lmapToList f (a:.b) = (f a : lmapToList f b)
lmapToList f a = [f a]


isTrue :: Environment -> LValue -> IO Bool
isTrue env arg =  (eval env arg) >>= (isTrue' env) where
    isTrue' env (Nil) = return False
    isTrue' env (a:.b) = return True
    isTrue' env (Number 0) = return False
    isTrue' env (Number _) = return True
    isTrue' env thing = lispEval(env, thing) >>= (\(_, y) -> case y of
        (Atom a) -> if inscope env a then isTrue env (unsafeLookup a env) else return True
        a -> isTrue env a)


eval :: Environment -> LValue -> IO LValue
eval env thing = fmap snd $ lispEval(env, thing)



listDo :: Environment -> LValue -> IO (Environment, LValue)
listDo env (Atom "define":.Atom name:.body:.Nil) = return (updateEnvironment env (name, body), nil)

listDo env (Atom "if":.cond:.thing1:.thing2:.Nil) = (isTrue env cond) >>= \p -> if p then lispEval(env, thing1) else lispEval(env, thing2)

listDo env (Atom "quote":.stuff) = return (env, stuff)
listDo env (Atom "lambda":.args:.body:.Nil) = return (env, createFunction env args body)
listDo env (Atom "assign":.Atom name:.body:.Nil) =  fmap (\(_, thing) -> (updateEnvironment env (name, thing), thing)) $ lispEval(env, body)
listDo env (Atom name:.stuff) = case unsafeLookup name env of
    (LFunction func _) -> fmap ((,) env) (func env $ fmap lispList (flipListIO $ map (eval env) (haskList stuff)))
       
    a -> return (env, (a:.stuff))

listDo env ((argh:.blah):.rest) = listDo env (argh:.blah) >>= (\(_, thing) -> case thing of
    LFunction func _ ->  fmap ((,) env) (func env $ fmap lispList (flipListIO $ map (eval env) (haskList rest)))
    a -> return (env, (a:.rest)))
listDo env a = return (env, a)


lispEval :: (Environment, LValue) -> IO (Environment, LValue)
lispEval (env, (Atom "quote":.thing)) = return (env, thing)
lispEval (env, Atom a) | inscope env a = lispEval(env, unsafeLookup a env)
    | otherwise = return (env, Atom a)
lispEval (env, (thing:.things)) = listDo env (thing:.things)
lispEval (env, a) = return (env, a)


flipListIO :: [IO a] -> IO [a]
flipListIO (x:xs) = x >>= (\a -> fmap (a :) (flipListIO xs))
flipListIO [] = return []

execExpr :: IO Environment -> LValue -> IO Environment
execExpr foo val = foo >>= \env -> fmap fst $ lispEval (env, val)

--putInto :: [LValue] -> LValue
--putInto (arg:[]) = arg
--putInto (arg:args) = Cons (arg:args)

reverseTuple (a, b) = (b, a)
