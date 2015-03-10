module Builtins where
import EvalLisp
import ParseLisp


repl = repl' "stdin" stdEnv

repl' :: String -> Environment -> IO ()
repl' fname env = do
    putStr "hank-lisp> "
    line <- getLine
    handleCommands (env, fname) line (\str -> do
            let str' = (parseString str)
            (env', value) <- if null str' then return (env, nil) else lispEval(env, tok2Val $ head str')
            case value == nil of
                True -> repl' fname env'
                False -> do
                    print value
                    repl' fname $ ("it", value) : env')

loadForRepl :: String -> IO ()
loadForRepl fname = do
    text <- readFile fname
    let trees = toks2Vals $ parseString text
    let env = execBlocks stdEnv trees
    env >>= (repl' fname)

execBlocks :: Environment -> [LValue] -> IO Environment
execBlocks env trees = foldl execExpr (return env) trees

handleCommands :: (Environment, String) -> String -> (String -> IO ()) -> IO ()
handleCommands (env, fname) str alt | null str = alt str
    | head str == ':' = case tail str of
        [] -> putStrLn "unrecognized command" >> alt ""
        "d" -> printDir env >> alt ""
        "dir" -> printDir env >> alt ""
        "reload" -> putStrLn "reloaded files" >> if (fname == "stdin") then repl else loadForRepl fname
        "r" -> putStrLn "reloaded files" >> if (fname == "stdin") then repl else loadForRepl fname
        "q" -> putStrLn "Leaving HankLisp."
        a -> putStrLn ("unknown command " ++ a) >> alt ""
    | otherwise = alt str

stdEnv :: Environment
stdEnv = [("+", LFunction lPlus "(+ x1 x2... xn) -> sum of x1 thru xn"),
    ("*", LFunction lTimes "(* x1 x2... xn) -> product of x1 thru xn"),
    ("eval", LFunction lEval "(eval something) -> unquoted 'something'"),
    ("car", LFunction lCar "(car a b) -> a, or (car (a b)) -> a"),
    ("cdr", LFunction lCdr "(cdr a b) -> (b), or (cdr (a b)) -> (b)"),
    ("cons", LFunction lCons "(cons x1 x2... xn) -> (x1 x2... xn), not evaluating"),
    ("get", LFunction lGetLine "(get) -> a single string from input"),
    ("putLn", LFunction lPutLine "(putLn x1 x2... xn) -> writes x1 x2.. xn to stdout, separated by spaces"),
    ("nil", Cons []),
    ("=", LFunction lEq "(= x1 x2... xn) -> T if all are equal, else nil"),
    ("eq", LFunction lEq "(eq x1 x2... xn) -> T if all are equal, else nil"),
    ("write", LFunction lWriteFile "(write fname x1... xn) -> writes x1 thru xn joined by spaces to fname"),
    ("read", LFunction lReadFile "(read fname) -> reads from fname"),
    ("_", Str " "),
    ("concat", LFunction lConcat "(concat s1 s2..sn) -> s1 thru sn concatenated without spaces"),
    ("stringP", LFunction lStringP "(stringP arg) -> whether arg is a string"),
    ("toString", LFunction lToString "(toString arg) -> a string for arg. Not reversable."),
    ("flat", LFunction lFlat "(flat a1 a2... an) -> a list of all the arguments, with lists flattened"),
    ("null", LFunction lNull "(null arg) -> whether arg is null"),
    ("not", LFunction lNot "(not arg) -> logical negation of arg")]


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
lCar env = fmap (\stuff -> case stuff of
    (a:rest:[]) -> a
    (Cons (a:as):rest) -> a
    (a:rest) -> a)

lCdr :: LFunctionT
lCdr env = fmap (\stuff -> case stuff of
    (Cons (a:as):rest) -> Cons as
    (a:rest) -> Cons rest)

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

lConcat :: LFunctionT
lConcat env = fmap (\args -> Str $ concat (map extract args))

lNull :: LFunctionT
lNull env = fmap (\args -> case args of
    (Cons []):other -> Atom "T"
    other -> Cons [])

lNot :: LFunctionT
lNot = lNull


lStringP :: LFunctionT
lStringP env = fmap (\args -> case args of
    (Str a):other -> Atom "T"
    other -> nil)

lToString :: LFunctionT
lToString env = fmap (\args -> case args of
    (Str s):other -> Str s
    (Atom a):other -> Str a
    (Cons s):other -> Str $ show s
    (Number i):other -> Str $ show i)

lFlat :: LFunctionT
lFlat env = fmap (\args -> Cons $ concat (map toList args)) where
    toList :: LValue -> [LValue]
    toList (Cons stuff) = stuff
    toList a = [a]



printDir :: Environment -> IO ()
printDir env = putStrLn (unlines $ map (\(x, y) -> x ++ " " ++ show y) env)



lReadFile :: LFunctionT
lReadFile env args = args >>= (\args' -> case args' of
    (Str fname):other -> fmap Str (readFile fname)
    badArg:other -> return $ Str ("Failure: " ++ show badArg ++ " is not a string or atom")
    [] -> return $ Str "Failure: no file specified to be read")

lWriteFile :: LFunctionT
lWriteFile env args = args >>= (\args' -> case args' of
    (Str fname):other -> writeFile fname (unwords $ map extract other) >> return nil
    badArg:blah:other -> return $ Str ("Failure: " ++ show badArg ++ " is not a string or atom")
    [] -> return $ Str "Failure: no file specified to be written")
