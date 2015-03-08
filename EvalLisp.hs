module EvalLisp where

import ParseLisp
import Control.Applicative
import Control.Monad (join)

type Environment = [(String, LValue)]

type LFunctionT = Environment -> IO [LValue] -> IO LValue

data LValue = Atom String |
    Number Integer |
    Cons [LValue] |
    LFunction LFunctionT |
    Str String

instance Show LValue where
    show (Atom str) = str
    show (Number i) = show i
    show (Cons stuff) = "S" ++ show stuff
    show (LFunction func) = "<function>"
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

toks2Vals = map tok2Val

nil = Atom "nil"

unsafeLookup a b = case lookup a b of
    Just a -> a
    Nothing -> error $ show a ++ " not in scope"

updateEnvironment :: Environment -> (String, LValue) -> Environment
updateEnvironment = flip (:)


deref :: Environment -> LValue -> LValue
deref env (Atom name) | inscope env name = unsafeLookup name env
    | otherwise = Atom name
deref env (Number i) = Number i

createFunction :: Environment -> LValue -> LValue -> LValue
createFunction env (Cons args) body = LFunction lispFunction where
    lispFunction :: LFunctionT
    lispFunction env args' = args' >>= (\args''-> fmap snd $ lispEval(zip (map extract args) args'' ++ env, body))



inscope env name = case lookup name env of
    Just _ -> True
    Nothing -> False

lPlus :: LFunctionT
lPlus env = fmap (\args -> case args of
    [] -> Number 0
    nums ->  foldr (\(Number x) (Number y) -> Number (x + y)) (Number 0) nums)

lTimes :: LFunctionT
lTimes env = fmap (\args -> case args of
    [] -> Number 1
    nums -> foldr (\(Number x) (Number y) -> Number (x * y)) (Number 1) nums)

lEval :: LFunctionT
lEval env arghs = arghs >>= (\a -> case a of
    thing:_ -> eval env thing)


lCar :: LFunctionT
lCar env = fmap (\(Cons (a:as):args) -> a)

lCdr :: LFunctionT
lCdr env = fmap (\(Cons (a:as):args) -> Cons as)

lCons :: LFunctionT
lCons env = fmap Cons

lGetLine :: LFunctionT
lGetLine env args = fmap Str getLine

lPutLine :: LFunctionT
lPutLine env args = args >>= (\args' -> fmap (const nil) $ putStrLn (unwords $ map extract args'))

lEq :: LFunctionT
lEq env = fmap (\args ->
    let
        blah (a:b:[]) = a == b
        blah (a:b:xs) = (a == b) && (blah (b:xs))
    in case blah args of
        True -> Atom "T"
        False -> Cons [])
    

stdEnv :: Environment
stdEnv = [("+", LFunction lPlus),
    ("*", LFunction lTimes),
    ("eval", LFunction lEval),
    ("car", LFunction lCar),
    ("cdr", LFunction lCdr),
    ("cons", LFunction lCons),
    ("get", LFunction lGetLine),
    ("putLn", LFunction lPutLine),
    ("nil", Cons []),
    ("=", LFunction lEq),
    ("eq", LFunction lEq)]

isTrue :: Environment -> LValue -> IO Bool
isTrue env (Cons []) = return False
isTrue env (Number 0) = return False
isTrue env (Number _) = return True
isTrue env (Atom "nil") = return False
isTrue env thing = lispEval(env, thing) >>= \(_, y) -> isTrue env y


eval :: Environment -> LValue -> IO LValue
--eval env (Cons (Atom "quote":thing:[])) = return thing
--eval env (Cons (Atom "quote":things)) = return $ Cons things
eval env thing = fmap snd $ lispEval(env, thing)



listDo :: Environment -> [LValue] -> IO (Environment, LValue)
listDo env (Atom "define":Atom name:body:[]) = return (updateEnvironment env (name, body), nil)

listDo env (Atom "if":cond:thing1:thing2:[]) = (isTrue env cond) >>= \p -> if p then lispEval(env, thing1) else lispEval(env, thing2)

listDo env (Atom "quote":stuff) = do
    putStrLn $ "evaluating " ++ show stuff
    return (env, Cons (Atom "quote":stuff))
listDo env (Atom "lambda":args:body:[]) = return (env, createFunction env args body)
listDo env (Atom "assign":Atom name:body:[]) =  fmap (\(_, thing) -> (updateEnvironment env (name, thing), thing)) $ lispEval(env, body)
listDo env (Atom name:stuff) = case unsafeLookup name env of
    (LFunction func) -> fmap (\a -> (env, a)) (func env (flipListIO $ map (eval env) stuff))
       
    a -> return (env, a)

listDo env (Cons stuff:rest) = listDo env stuff >>= \(_, LFunction func) -> fmap (\a -> (env, a)) (func env (flipListIO $ map (eval env) rest))
listDo env (Number i:rest) = return (env, Cons (Number i : rest))
listDo env [] = return (env, Cons [])
listDo env a = error $ "wtf " ++ show a


lispEval :: (Environment, LValue) -> IO (Environment, LValue)
lispEval (env, Cons (Atom "quote":thing:[])) = return (env, thing)
lispEval (env, Cons (Atom "quote":things)) = return (env, Cons things)
lispEval (env, Cons thing) = listDo env thing
lispEval (env, Atom a) | inscope env a = lispEval(env, deref env $ Atom a)
    | otherwise = return (env, Atom a)
lispEval (env, a) = return (env, a)


flipListIO :: [IO a] -> IO [a]
flipListIO (x:xs) = x >>= (\a -> fmap (a :) (flipListIO xs))
flipListIO [] = return []

execExpr :: IO Environment -> LValue -> IO Environment
execExpr foo val = foo >>= \env -> fmap fst $ lispEval (env, val)

repl = repl' stdEnv

repl' env = do
    --putStr "hank-lisp> "
    str <- getLine
    putStr "hank-lisp> "
    let str' = (parseString str)
    (env', value) <- if null str' then return (env, nil) else lispEval(env, tok2Val $ head str')
    case value == nil of
        True -> repl' env'
        False -> do
            print value
            repl' $ ("it", value) : env'

loadForRepl :: String -> IO ()
loadForRepl fname = do
    text <- readFile fname
    let trees = toks2Vals $ parseString text
    let env = foldl execExpr (return stdEnv) trees
    env >>= repl'



reverseTuple (a, b) = (b, a)