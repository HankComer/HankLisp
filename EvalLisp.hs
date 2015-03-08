module EvalLisp where

import ParseLisp
import Control.Applicative
import Control.Monad (join)

type Environment = [(String, LValue)]

type LFunctionT = Environment -> IO [LValue] -> IO LValue

data LValue = Atom String |
    Number Integer |
    Cons [LValue] |
    LFunction LFunctionT String |
    Str String

instance Show LValue where
    show (Atom str) = str
    show (Number i) = show i
    show (Cons stuff) = "S" ++ show stuff
    show (LFunction func meta) = "function " ++ meta
    show (Str str) = show str

instance Eq LValue where
    (Atom a) == (Atom b) = a == b
    (Number i) == (Number j) = i == j
    (Cons a) == (Cons b) = a == b
    (Str a) == (Str b) = a == b
    _ == _ = False

extract :: LValue -> String
extract (Atom a) = a
extract (Str a) = a
extract (Number n) = show n

tok2Val :: LToken -> LValue
tok2Val (LAtom a) = Atom a
tok2Val (LInt i) = Number i
tok2Val (LList ts) = Cons (map tok2Val ts)
tok2Val (LString s) = Str s

toks2Vals = map tok2Val

nil = Cons []

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
createFunction env (Cons args) body = LFunction lispFunction metaData where
    lispFunction :: LFunctionT
    lispFunction env' args' = args' >>= (\args''-> fmap snd $ lispEval(zip (map extract args) args'' ++ env', body))
    metaData :: String
    metaData = show args ++ " -> " ++ show body



inscope env name = case lookup name env of
    Just _ -> True
    Nothing -> False


resolve :: Environment -> String -> LValue
resolve env str = case lookup str env of
    Just (Atom a) -> if a == str then resolve (tail env) str else Atom a
    Just other -> other
    Nothing -> Atom str



isTrue :: Environment -> LValue -> IO Bool
isTrue env arg =  (eval env arg) >>= (isTrue' env) where
    isTrue' env (Cons []) = return False
    isTrue' env (Cons a) = return True
    isTrue' env (Number 0) = return False
    isTrue' env (Number _) = return True
    isTrue' env thing = lispEval(env, thing) >>= (\(_, y) -> case y of
        (Atom a) -> if inscope env a then isTrue env (resolve env a) else return True
        a -> isTrue env a)


eval :: Environment -> LValue -> IO LValue
--eval env (Cons (Atom "quote":thing:[])) = return thing
--eval env (Cons (Atom "quote":things)) = return $ Cons things
eval env thing = fmap snd $ lispEval(env, thing)



listDo :: Environment -> [LValue] -> IO (Environment, LValue)
listDo env (Atom "define":Atom name:body:[]) = return (updateEnvironment env (name, body), nil)

listDo env (Atom "if":cond:thing1:thing2:[]) = (isTrue env cond) >>= \p -> if p then lispEval(env, thing1) else lispEval(env, thing2)

listDo env (Atom "quote":stuff) = return (env, Cons (Atom "quote":stuff))
listDo env (Atom "lambda":args:body:[]) = return (env, createFunction env args body)
listDo env (Atom "assign":Atom name:body:[]) =  fmap (\(_, thing) -> (updateEnvironment env (name, thing), thing)) $ lispEval(env, body)
listDo env (Atom name:stuff) = case unsafeLookup name env of
    (LFunction func _) -> fmap (\a -> (env, a)) (func env (flipListIO $ map (eval env) stuff))
       
    a -> return (env, Cons (a:stuff))

listDo env (Cons stuff:rest) = listDo env stuff >>= (\(_, thing) -> case thing of
    LFunction func _ -> fmap (\a -> (env, a)) (func env (flipListIO $ map (eval env) rest))
    a -> return (env, Cons (a:rest)))
listDo env (Number i:rest) = return (env, Cons (Number i : rest))
listDo env [] = return (env, Cons [])
listDo env a = error $ "wtf " ++ show a


lispEval :: (Environment, LValue) -> IO (Environment, LValue)
lispEval (env, Cons (Atom "quote":thing:[])) = return (env, thing)
lispEval (env, Cons (Atom "quote":things)) = return (env, Cons things)
lispEval (env, Cons thing) = listDo env thing
lispEval (env, Atom a) | inscope env a = lispEval(env, unsafeLookup a env)
    | otherwise = return (env, Atom a)
lispEval (env, a) = return (env, a)


flipListIO :: [IO a] -> IO [a]
flipListIO (x:xs) = x >>= (\a -> fmap (a :) (flipListIO xs))
flipListIO [] = return []

execExpr :: IO Environment -> LValue -> IO Environment
execExpr foo val = foo >>= \env -> fmap fst $ lispEval (env, val)



reverseTuple (a, b) = (b, a)